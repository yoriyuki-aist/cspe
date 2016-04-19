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

import java.util.HashMap;

public class QeaMonitor {
    public static int ACCESS = 1;
    public static int OPEN = 2;
    public static int CLOSE = 3;
    public static int SPAWN = 4;
    public static int EXIT = 5;

    public static int pid = 1;
    public static int parent = 2;
    public static int child = 3;
    public static int current = 4;
    public static int a = 5;
    public static int b = 6;

    private HashMap<Integer, Integer> integerPool = new HashMap();
    private Integer intern(int v) {
        Integer v1 = v;
        Integer v2 = integerPool.get(v1);

        if (v2 != null) {return v2;} else {
            integerPool.put(v1, v1);
            return v1;
        }
    }

    private QEA fdQEA;
    private Monitor<QEA> fdMonitor;

    private QEA processQEA;
    private Monitor<QEA> processMonitor;

    private void makefdMonitor() {
        int fd = -1;

        QEABuilder q = new QEABuilder("Fd Open-Close Matching");

        q.setNegated(true);
        q.addQuantification(EXISTS, fd);

        //1 : process 0
        //2 : fd not yet opened
        //3 : fd opened
        //4 : in child process
        //5 : error
        //6 : success <- not used
        q.addTransition(1, SPAWN, new int[]{parent, child}, varIsEqualToIntVal(parent, 0), storeVar(pid, child), 2);
        q.addTransition(1, SPAWN, new int[]{parent, child}, varIsEqualToIntVal(parent, 0), 1);
        q.addTransition(1, SPAWN, new int[]{parent, child}, not(varIsEqualToIntVal(parent, 0)), 1);
        q.addTransition(1, OPEN, new int[]{a, fd}, varIsEqualToIntVal(a, 0), storeVar(pid, a), 3);
        q.addTransition(1, ACCESS, new int[]{a, fd}, varIsEqualToIntVal(a, 0), 5);
        q.addTransition(1, ACCESS, new int[]{a, fd}, not(varIsEqualToIntVal(a, 0)), 1);
        q.addTransition(1, CLOSE, new int[]{a, fd}, varIsEqualToIntVal(a, 0), 5);
        q.addTransition(1, CLOSE, new int[]{a, fd}, not(varIsEqualToIntVal(a, 0)), 1);
        q.addTransition(1, EXIT, new int[]{a}, not(varIsEqualToIntVal(a, 0)), 1);
        q.addTransition(1, EXIT, new int[]{a}, varIsEqualToIntVal(a, 0), 5);

        q.addTransition(2, OPEN, new int[]{a, fd}, isEqual(a, pid), 3);
        q.addTransition(2, OPEN, new int[]{a, fd}, not(isEqual(a, pid)), 2);
        q.addTransition(2, CLOSE, new int[]{a, fd}, isEqual(a, pid), 5);
        q.addTransition(2, CLOSE, new int[]{a, fd}, not(isEqual(a, pid)), 2);
        q.addTransition(2, ACCESS, new int[]{a, fd}, isEqual(a, pid), 5);
        q.addTransition(2, ACCESS, new int[]{a, fd}, not(isEqual(a, pid)), 2);
        q.addTransition(2, SPAWN, new int[]{parent, child}, isEqual(parent, pid), 2);
        q.addTransition(2, SPAWN, new int[]{parent, child}, isEqual(parent, pid), storeVar(pid, child), 2);
        q.addTransition(2, SPAWN, new int[]{parent, child}, not(isEqual(parent, pid)), 2);

        q.addTransition(3, OPEN, new int[]{a, fd}, isEqual(a, pid), 5);
        q.addTransition(3, OPEN, new int[]{a, fd}, not(isEqual(a, pid)), 3);
        q.addTransition(3, CLOSE, new int[]{a, fd}, isEqual(a, pid), 2);
        q.addTransition(3, CLOSE, new int[]{a, fd}, not(isEqual(a, pid)), 3);
        q.addTransition(3, ACCESS, new int[]{a, fd}, 3);
        q.addTransition(3, SPAWN, new int[]{parent, child}, isEqual(parent, pid), 3);
        q.addTransition(3, SPAWN, new int[]{parent, child}, isEqual(parent, pid), storeVar(pid, child), 4);
        q.addTransition(3, SPAWN, new int[]{parent, child}, not(isEqual(parent, pid)), 3);
        q.addTransition(3, EXIT, new int[]{a}, isEqual(a, pid), 5);
        q.addTransition(3, EXIT, new int[]{a}, not(isEqual(a, pid)), 3);

        q.addTransition(4, OPEN, new int[]{a, fd}, isEqual(a, pid), 5);
        q.addTransition(4, OPEN, new int[]{a, fd}, not(isEqual(a, pid)), 4);
        q.addTransition(4, CLOSE, new int[]{a, fd}, isEqual(a, pid), 2);
        q.addTransition(4, CLOSE, new int[]{a, fd}, not(isEqual(a, pid)), 4);
        q.addTransition(4, ACCESS, new int[]{a, fd}, 4);
        q.addTransition(4, SPAWN, new int[]{parent, child}, isEqual(parent, pid), 4);
        q.addTransition(4, SPAWN, new int[]{parent, child}, isEqual(parent, pid), storeVar(pid, child), 4);
        q.addTransition(4, SPAWN, new int[]{parent, child}, not(isEqual(parent, pid)), 4);
        q.addTransition(4, EXIT, new int[]{a}, not(isEqual(a, pid)), 4);
        q.addTransition(4, EXIT, new int[]{a}, isEqual(a, pid), 5);

        q.setSkipStates(5); //using skip states makes the monitor very slow
        q.addFinalStates(5);

        fdQEA = q.make();
    }

    private void makeProcessMonitor(){
        int processSet = 7;

        QEABuilder q = new QEABuilder("Process Spawn-Exit Matching");

        q.addTransition(1, SPAWN, new int[]{parent, child},
                        and(or(setContainsElement(parent, processSet), varIsEqualToIntVal(parent, 0)),
                                not(setContainsElement(child, processSet))),
                        addElementToSet(processSet, child), 1);
        q.addTransition(1, EXIT, new int[]{pid}, setContainsElement(pid, processSet),
                        removeElementFromSet(processSet, pid), 1);
        q.addTransition(1, OPEN, new int[]{a, b}, 1);
        q.addTransition(1, CLOSE, new int[]{a, b}, 1);
        q.addTransition(1, ACCESS, new int[]{a, b}, 1);

        q.addFinalStates(1);

        processQEA = q.make();
    }

    public QeaMonitor() {
        makefdMonitor();
        fdMonitor = MonitorFactory.create(fdQEA);

        makeProcessMonitor();
        processMonitor = MonitorFactory.create(processQEA);
    }

    public boolean step(int event, int arg){
        Verdict v1 = fdMonitor.step(event, intern(arg));
        Verdict v2 = processMonitor.step(event, intern(arg));
 //       System.out.println(v1.toString() + " and " + v2.toString());

        return ((v1 != Verdict.FAILURE) && (v2 != Verdict.FAILURE));
    }

    public boolean step(int event, int arg1, int arg2){
        Integer n1 = intern(arg1);
        Integer n2 = intern(arg2);

        Verdict v1 = fdMonitor.step(event, n1, n2);
        Verdict v2 = processMonitor.step(event, n1, n2);
  //      System.out.println(v1.toString() + " and " + v2.toString());

        return ((v1 != Verdict.FAILURE) && (v2 != Verdict.FAILURE));
    }
}
