/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe;/**
 * Created by yoriyuki on 2016/04/04.
 */

//For specifying the QEA
import static qea.structure.impl.other.Quantification.EXISTS;
import static qea.structure.impl.other.Quantification.FORALL;

import qea.monitoring.impl.MonitorFactory;
import qea.monitoring.intf.Monitor;
import qea.structure.intf.QEA;
import qea.creation.QEABuilder;
import static qea.structure.intf.Guard.*;
import static qea.structure.intf.Assignment.*;

//For performing monitoring
import qea.monitoring.impl.FileMonitor;
import qea.monitoring.impl.CSVFileMonitor;
import qea.monitoring.impl.translators.OfflineTranslator;
import qea.monitoring.impl.translators.DefaultTranslator;
import qea.structure.impl.other.Verdict;

public class QeaMonitor {
    public static int ACCESS = 1;
    public static int OPEN = 2;
    public static int CLOSE = 3;
    public static int SPAWN = 4;
    public static int EXIT = 5;

    public static int parent = 1;
    public static int child = 2;
    public static int current = 3;
    public static int a = 4;
    public static int b = 5;

    private QEA qEA;
    private Monitor<QEA> monitor;

    private void makeMonitor() {
        int pid = -1;
        int fd = -2;

        QEABuilder q = new QEABuilder("Fd Open-Close Matching");

        q.setNegated(true);
        q.addQuantification(EXISTS, pid);
        q.addQuantification(EXISTS, fd);

        //1 : process not spawned
        //2 : fd not yet opened
        //3 : fd opened
        //4 : error
        q.addTransition(1, SPAWN, new int[]{parent, pid}, 2);
        q.addTransition(1, OPEN, new int[]{pid, fd}, varIsEqualToIntVal(pid, 0), 3);
        q.addTransition(1, OPEN, new int[]{pid, fd}, not(varIsEqualToIntVal(pid, 0)), 4);
        q.addTransition(1, ACCESS, new int[]{pid, fd}, 4);
        q.addTransition(1, CLOSE, new int[]{pid, fd}, 4);
        q.addTransition(1, EXIT, new int[]{pid}, 4);

        q.addTransition(2, OPEN, new int[]{pid, fd}, 3);
        q.addTransition(2, EXIT, new int[]{pid}, 1);
        q.addTransition(2, CLOSE, new int[]{pid, fd}, 4);
        q.addTransition(2, SPAWN, new int[]{parent, pid}, 4);
        q.addTransition(2, SPAWN, new int[]{pid, child}, 2);

        q.addTransition(3, OPEN, new int[]{pid, fd}, 4);
        q.addTransition(3, CLOSE, new int[]{pid, fd}, 2);
        q.addTransition(3, ACCESS, new int[]{pid, fd}, 3);
        q.addTransition(3, SPAWN, new int[]{parent, pid}, 4);
        q.addTransition(3, SPAWN, new int[]{pid, child}, 3);
        q.addTransition(3, EXIT, new int[]{pid}, 4);

        q.setSkipStates(4);
        q.addFinalStates(4);

        qEA = q.make();
    }

    public QeaMonitor() {
        makeMonitor();
        monitor = MonitorFactory.create(qEA);
    }

    public Verdict step(int event, int arg){
        return monitor.step(event, arg);
    }

    public Verdict step(int event, int arg1, int arg2) {
        return monitor.step(event, arg1, arg2);
    }
}
