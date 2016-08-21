/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe._
import jp.go.aist.cspe.CSPE._
import jp.go.aist.cspe.TraceFactory._
import qea.structure.impl.other.Verdict
/**
  * Created by yoriyuki on 2016/03/30.
  */

object MotivatingExample extends ExampleTrait {

  var max_pid = 0
  var max_fd = 2

  def eventStream(pid: Int, openFiles: Set[Int]): Stream[AbsEvent] =
    TraceFactory.choice {
      (if (openFiles.isEmpty) {
        if (pid == 0) Array.empty else Array(Event('Exit, pid) #:: skip)
      } else {
        Array(Event('Access, pid, randomChoice(openFiles)) #:: eventStream(pid, openFiles), {
          val fd = randomChoice(openFiles)
          Event('Close, pid, fd) #:: eventStream(pid, openFiles - fd)
        }
        )
      }) ++
        Array(
          {
            val fd = 1 + max_fd
            max_fd += 1
            Event('Open, pid, fd) #:: eventStream(pid, openFiles + fd)
          }, {
            val child_pid = 1 + max_pid
            max_pid += 1
            Event('Spawn, pid, child_pid) #::
              interleaving(eventStream(pid, openFiles), eventStream(child_pid, openFiles))
          }) ++
        (if (pid == 0) Array.empty[Stream[AbsEvent]] else Array(skip))
    }

  def process(pid: Int, openFiles: Set[Int]): Process = ?? {
    case Event('Spawn, `pid`, child_pid: Int) =>
      process(pid, openFiles) ||| process(child_pid, openFiles)
    case Event('Open, `pid`, fd: Int) if !openFiles(fd) =>
      process(pid, openFiles + fd)
    case Event('Access, `pid`, fd: Int) if openFiles(fd) =>
      process(pid, openFiles)
    case Event('Close, `pid`, fd: Int) if openFiles(fd) =>
      process(pid, openFiles - fd)
    case Event('Exit, `pid`) if pid != 0 && openFiles.isEmpty => SKIP
  }

  def uniqProcess(pidSet : Set[Int]) : Process = ?? {
    case Event('Spawn, _, child_pid : Int) if ! pidSet(child_pid) => uniqProcess(pidSet + child_pid)
    case Event('Exit, pid : Int) if pidSet(pid) => uniqProcess(pidSet - pid)
  }

  def system = process(0, Set.empty) || Set('Spawn, 'Exit) || uniqProcess(Set(0))

  def genEventStream(n : Int) = {
    val ret = eventStream(0, Set.empty).take(n).toList
    println("Processes: " + max_pid + " Fd: " + max_fd)
    ret
  }

  def createCSPEModel() : Process = system

  def createQeaModel() : QeaMonitor = new MotivatingExampleQeaMonitor()

  def debugCSPEModel(): Unit = {
    val monitors = new ProcessSet(List(system))

    //for debug
    assert(! (monitors |~ List(Event('Access, 0, 4))))
    assert(! (monitors |~ List(Event('Open, 0, 4), Event('Spawn, 0, 1), Event('Close, 0, 4), Event('Exit, 1))))
    assert(! (monitors |~ List(Event('Exit, 1))))
    assert(! (monitors |~ List(Event('Spawn, 0, 1), Event('Open, 1, 4), Event('Exit, 1))))
    assert(! (monitors |~ List(Event('Spawn, 0, 1), Event('Spawn, 1, 0))))
  }

    def debugQeaModel(): Unit = {
      // Should fail
      assert(! (new MotivatingExampleQeaMonitor() step(MotivatingExampleQeaMonitor.ACCESS, 0, 4)))
      val q1 = new MotivatingExampleQeaMonitor()
      q1.step(MotivatingExampleQeaMonitor.OPEN, 0, 4)
      q1.step(MotivatingExampleQeaMonitor.SPAWN, 0, 1)
      q1.step(MotivatingExampleQeaMonitor.CLOSE, 0, 4)
      assert(! q1.step(MotivatingExampleQeaMonitor.EXIT, 1))
      val q2 = new MotivatingExampleQeaMonitor()
      assert(! q2.step(MotivatingExampleQeaMonitor.EXIT, 1))
      val q3 = new MotivatingExampleQeaMonitor()
      q3.step(MotivatingExampleQeaMonitor.SPAWN, 0, 1)
      q3.step(MotivatingExampleQeaMonitor.OPEN, 1, 4)
      assert(! q3.step(MotivatingExampleQeaMonitor.EXIT, 1))
      val q8 = new MotivatingExampleQeaMonitor()
      q8.step(MotivatingExampleQeaMonitor.SPAWN, 0, 1)
      assert(! q8.step(MotivatingExampleQeaMonitor.SPAWN, 1, 0))


      // Should success
      val q4 = new MotivatingExampleQeaMonitor()
      q4.step(MotivatingExampleQeaMonitor.SPAWN, 0, 1)
      q4.step(MotivatingExampleQeaMonitor.OPEN, 1, 1)
      q4.step(MotivatingExampleQeaMonitor.ACCESS, 1, 1)
      q4.step(MotivatingExampleQeaMonitor.CLOSE, 1, 1)
      assert(q4.step(MotivatingExampleQeaMonitor.EXIT, 1))

      val q5 = new MotivatingExampleQeaMonitor()
      q5.step(MotivatingExampleQeaMonitor.SPAWN, 0, 1)
      q5.step(MotivatingExampleQeaMonitor.OPEN, 1, 1)
      q5.step(MotivatingExampleQeaMonitor.CLOSE, 1, 1)
      assert(q5.step(MotivatingExampleQeaMonitor.EXIT, 1))

      val q6 = new MotivatingExampleQeaMonitor()
      q6.step(MotivatingExampleQeaMonitor.OPEN, 0, 1372)
      q6.step(MotivatingExampleQeaMonitor.SPAWN, 0, 1371)
      assert(q6.step(MotivatingExampleQeaMonitor.ACCESS, 0, 1372))

      val q7 = new MotivatingExampleQeaMonitor()
      q7.step(MotivatingExampleQeaMonitor.OPEN, 0, 2)
      q7.step(MotivatingExampleQeaMonitor.SPAWN, 0, 1)
      assert(q7.step(MotivatingExampleQeaMonitor.ACCESS, 0, 2))
    }

    def symbolMap = Map(
      'Access -> 1,
      'Open -> 2,
      'Close -> 3,
      'Spawn -> 4,
      'Exit -> 5
  )
}
