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

    public static int pid = -1;
    public static int fd = -2;

    public static int parent = 1;
    public static int child = 2;
    public static int current = 3;
    public static int a = 4;
    public static int b = 5;

    private QEA fdMatchingQEA;
    private QEA accessQEA;
    private Monitor<QEA> fdMatchingMonitor;
    private Monitor<QEA> accessMonitor;

    private void makeFdMatchingMonitor() {

        QEABuilder q = new QEABuilder("Fd Open-Close Matching");

        q.setNegated(true);
        q.addQuantification(EXISTS, pid);
        q.addQuantification(EXISTS, fd);

        q.addTransition(1, SPAWN, new int[]{parent, pid}, 2);
        q.addTransition(2, CLOSE, new int[]{pid, fd}, 3);
        q.addTransition(2, OPEN, new int[]{pid, fd}, 4);
        q.addTransition(4, EXIT, new int[]{pid}, 5);

        q.addFinalStates(3, 4);

        fdMatchingQEA = q.make();
    }

    private void makeAccessMonitor() {

        QEABuilder q = new QEABuilder("Access Monitor");

        q.addQuantification(FORALL, fd);

        q.addTransition(1, OPEN, new int[]{a, fd}, storeVar(current, a), 2);
        q.addTransition(2, ACCESS, new int[]{a, fd}, isEqual(a, current), 2);
        q.addTransition(2, SPAWN, new int[]{a, b}, isEqual(a, current), storeVar(current, b), 2);

        q.addFinalStates(2);
        q.setSkipStates(1, 2);

        accessQEA = q.make();
    }

    public QeaMonitor() {
        fdMatchingMonitor = MonitorFactory.create(fdMatchingQEA);
        accessMonitor = MonitorFactory.create(accessQEA);
    }

    public boolean step(int event, int arg){
        Verdict v1 = fdMatchingMonitor.step(event, arg);
        Verdict v2 = accessMonitor.step(event, arg);
        return !((v1 == Verdict.FAILURE) && (v2 == Verdict.FAILURE));
    }

    public boolean step(int event, int arg1, int arg2){
        Verdict v1 = fdMatchingMonitor.step(event, arg1, arg2);
        Verdict v2 = accessMonitor.step(event, arg1, arg2);
        return !((v1 == Verdict.FAILURE) || (v2 == Verdict.FAILURE));
    }
}
