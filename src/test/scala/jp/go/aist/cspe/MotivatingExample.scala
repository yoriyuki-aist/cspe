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

object MotivatingExample {

  def random[T](s: Set[T]): T = {
    val n = util.Random.nextInt(s.size)
    val it = s.iterator.drop(n)
    it.next()
  }

  var max_pid = 0
  var max_fd = 2

  def genEventStream(pid: Int, openFiles: Set[Int]): Stream[AbsEvent] =
    TraceFactory.choice {
      (if (openFiles.isEmpty) {
        if (pid == 0) Array.empty else Array(Event('Exit, pid) #:: skip)
      } else {
        Array(Event('Access, pid, random(openFiles)) #:: genEventStream(pid, openFiles), {
          val fd = random(openFiles)
          Event('Close, pid, fd) #:: genEventStream(pid, openFiles - fd)
        }
        )
      }) ++
        Array(
          {
            val fd = 1 + max_fd
            max_fd += 1
            Event('Open, pid, fd) #:: genEventStream(pid, openFiles + fd)
          }, {
            val child_pid = 1 + max_pid
            max_pid += 1
            Event('Spawn, pid, child_pid) #::
              interleving(genEventStream(pid, openFiles), genEventStream(child_pid, openFiles))
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

  def main(args: Array[String]) {
    val monitors = new ProcessSet(List(system))

    //for debug
    println(monitors << Event('Access, 0, 4))
    println(monitors << Event('Open, 0, 4) << Event('Spawn, 0, 1) << Event('Close, 0, 4) << Event('Exit, 1))
    println(monitors << Event('Exit, 1))
    println(monitors << Event('Spawn, 0, 1) << Event('Open, 1, 4) << Event('Exit, 1))
    println(monitors << Event('Spawn, 0, 1) << Event('Spawn, 1, 0))

    // Should fail
    println(new MotivaatingExampleQeaMonitor() step(MotivaatingExampleQeaMonitor.ACCESS, 0, 4))
    val q1 = new MotivaatingExampleQeaMonitor()
    q1.step(MotivaatingExampleQeaMonitor.OPEN, 0, 4)
    q1.step(MotivaatingExampleQeaMonitor.SPAWN, 0, 1)
    q1.step(MotivaatingExampleQeaMonitor.CLOSE, 0, 4)
    println(q1.step(MotivaatingExampleQeaMonitor.EXIT, 1))
    val q2 = new MotivaatingExampleQeaMonitor()
    println(q2.step(MotivaatingExampleQeaMonitor.EXIT, 1))
    val q3 = new MotivaatingExampleQeaMonitor()
    q3.step(MotivaatingExampleQeaMonitor.SPAWN, 0, 1)
    q3.step(MotivaatingExampleQeaMonitor.OPEN, 1, 4)
    println(q3.step(MotivaatingExampleQeaMonitor.EXIT, 1))
    val q8 = new MotivaatingExampleQeaMonitor()
    q8.step(MotivaatingExampleQeaMonitor.SPAWN, 0, 1)
    println(q8.step(MotivaatingExampleQeaMonitor.SPAWN, 1, 0))


    // Should success
    val q4 = new MotivaatingExampleQeaMonitor()
    q4.step(MotivaatingExampleQeaMonitor.SPAWN, 0, 1)
    q4.step(MotivaatingExampleQeaMonitor.OPEN, 1, 1)
    q4.step(MotivaatingExampleQeaMonitor.ACCESS, 1, 1)
    q4.step(MotivaatingExampleQeaMonitor.CLOSE, 1, 1)
    println(q4.step(MotivaatingExampleQeaMonitor.EXIT, 1))

    val q5 = new MotivaatingExampleQeaMonitor()
    q5.step(MotivaatingExampleQeaMonitor.SPAWN, 0, 1)
    q5.step(MotivaatingExampleQeaMonitor.OPEN, 1, 1)
    q5.step(MotivaatingExampleQeaMonitor.CLOSE, 1, 1)
    println(q5.step(MotivaatingExampleQeaMonitor.EXIT, 1))

    val q6 = new MotivaatingExampleQeaMonitor()
    q6.step(MotivaatingExampleQeaMonitor.OPEN, 0, 1372)
    q6.step(MotivaatingExampleQeaMonitor.SPAWN, 0, 1371)
    println(q6.step(MotivaatingExampleQeaMonitor.ACCESS, 0, 1372))

    val q7 = new MotivaatingExampleQeaMonitor()
    q7.step(MotivaatingExampleQeaMonitor.OPEN, 0, 2)
    q7.step(MotivaatingExampleQeaMonitor.SPAWN, 0, 1)
    println(q7.step(MotivaatingExampleQeaMonitor.ACCESS, 0, 2))

    val max_trace = genEventStream(0, Set.empty).take(300000).toList

    var qeaTime: List[Double] = List()
    var cspeTime: List[Double] = List()

    val qeaMonitor = new MotivaatingExampleQeaMonitor()
    val start_qea = System.nanoTime()
    var trace = max_trace
    for (i <- 1 to 30) {
      val chunk = trace.take(10000)
      trace = trace.drop(10000)

      for (e <- chunk) {
        val verdict = e match {
          case Event('Access, pid: Int, fd: Int) => qeaMonitor.step(MotivaatingExampleQeaMonitor.ACCESS, pid, fd)
          case Event('Open, pid: Int, fd: Int) => qeaMonitor.step(MotivaatingExampleQeaMonitor.OPEN, pid, fd)
          case Event('Close, pid: Int, fd: Int) => qeaMonitor.step(MotivaatingExampleQeaMonitor.CLOSE, pid, fd)
          case Event('Spawn, parent: Int, child: Int) => qeaMonitor.step(MotivaatingExampleQeaMonitor.SPAWN, parent, child)
          case Event('Exit, pid: Int) => qeaMonitor.step(MotivaatingExampleQeaMonitor.EXIT, pid)
        }
        assert(verdict)
      }
      val rap_qea = System.nanoTime()
      qeaTime = qeaTime :+ (rap_qea - start_qea) / scala.math.pow(10, 9)
    }

    trace = max_trace
    val start = System.nanoTime()
    var cspe_monitors: ProcessSet = new ProcessSet(List(system))

    for (i <- 1 to 30) {
      val chunk = trace.take(10000)
      trace = trace.drop(10000)

      for (e <- chunk) {
        cspe_monitors = cspe_monitors << e
      }
      val rap_cspe = System.nanoTime()
      cspeTime = cspeTime :+ (rap_cspe - start) / scala.math.pow(10, 9)
      //println(monitors)
    }
    assert(!cspe_monitors.isFailure)

    val rs = qeaTime zip cspeTime
    var count = 10000
    for (ret <- rs) {
      println(count + "," + ret._1 + "," + ret._2)
      count = count + 10000
    }
  }
}
