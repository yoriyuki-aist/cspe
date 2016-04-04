/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

/**
 * Created by yoriyuki on 2016/04/04.
 */

//For specifying the QEA
import static qea.structure.impl.other.Quantification.EXISTS;
import static qea.structure.impl.other.Quantification.FORALL;
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

public class qeaMonitor {
    int ACCESS = 1;
    int OPEN = 2;
    int CLOSE = 3;
    int SPAWN = 4;
    int EXIT = 5;

    int pid = -1;
    int fd = -2;

    int parent = 1;
    int child = 2;
    int current = 3;
    int a = 4;
    int b = 5;

    private QEA FdMatingMonitor;
    private QEA AccessMonitor;

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

        FdMatingMonitor = q.make();
    }

    private void makeAccessMonitor() {

        QEABuilder q = new QEABuilder("Access Monitor");

        q.addQuantification(FORALL, fd);

        q.addTransition(1, OPEN, new int[]{a, fd}, storeVar(current, a), 2);
        q.addTransition(2, ACCESS, new int[]{a, fd}, isEqual(a, current), 2);
        q.addTransition(2, SPAWN, new int[]{a, b}, isEqual(a, current), storeVar(current, b), 2);

        q.addFinalStates(2);
        q.setSkipStates(1, 2);

        AccessMonitor = q.make();
    }
}
