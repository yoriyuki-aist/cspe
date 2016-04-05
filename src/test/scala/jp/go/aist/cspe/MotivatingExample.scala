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
/**
  * Created by yoriyuki on 2016/03/30.
  */

object MotivatingExample {

  def random[T](s: Set[T]) : T = {
    val n = util.Random.nextInt(s.size)
    val it = s.iterator.drop(n)
    it.next()
  }

  var max_pid = 0
  var max_fd = 2

  def process(pid : Int, openFiles : Set[Int]) : Stream[AbsEvent] =
    genEventStream(pid, openFiles) ++ Event('Exit, pid) #:: skip

  def genEventStream(pid : Int, openFiles : Set[Int]) : Stream[AbsEvent] =
    TraceFactory.choice {
      Array(
        Event('Access, pid, random(openFiles)) #:: genEventStream(pid, openFiles), {
          val fd = 1 + max_fd
          max_fd += 1
          Event('Open, pid, fd) #:: genEventStream(pid, openFiles + fd) ++
            Event('Close, pid, fd) #:: genEventStream(pid, openFiles)
        },
        {
        val child_pid = 1 + max_pid
        max_pid += 1
        Event('Spawn, pid, child_pid) #::
          interleving(genEventStream(pid, openFiles), process(child_pid, openFiles))
      }) ++
        (if (pid == 0) Array.empty[Stream[AbsEvent]] else Array(skip))
    }

  def childProcess(pid : Int, openFiles : Set[Int]) : Process =
    run(pid, openFiles) $ Event('Exit, `pid`) ->: SKIP

  def run(pid : Int, openFiles : Set[Int]) : Process = ?? {
    case Event('Access, `pid`, fd : Int) if openFiles(fd) =>
      run(pid, openFiles)
    case Event('Open, `pid`, fd : Int) if !openFiles(fd) =>
      run(pid, openFiles + fd) $ Event('Close, pid, fd) ->: run(pid, openFiles)
    case Event('Spawn, `pid`, child_pid : Int) =>
      run(pid, openFiles) ||| childProcess(child_pid, openFiles)
  } <+> SKIP

  def main(args: Array[String]) {
    var monitors = new ProcessSet(List(run(0, Set(0, 1, 2))))

    //for debug
    println(monitors << Event('Access, 0, 4))
    println(monitors << Event('Open, 0, 4) << Event('Spawn, 0, 1) << Event('Close, 1, 4))
    println(monitors << Event('Exit, 1))
    println(monitors << Event('Spawn, 0, 1) << Event('Open, 1, 4) << Event('Exit, 1))

    val trace = process(0, Set(0, 1, 2)).take(10000)

    val start = System.nanoTime()
    for(e <- trace) {
      monitors = monitors << e
    }
    val stop = System.nanoTime()
    println(monitors)
    println("CSP_E Elapsed: " + (stop - start) / scala.math.pow(10, 9) + "s")

    val qeaMonitor = new QeaMonitor()
    val start_qea = System.nanoTime()
    for(e <- trace) {
      val verdict = e match {
        case Event('Access, pid : Int, fd : Int) => qeaMonitor.step(QeaMonitor.ACCESS, pid, fd)
        case Event('Open, pid : Int, fd : Int) => qeaMonitor.step(QeaMonitor.OPEN, pid, fd)
        case Event('Close, pid : Int, fd : Int) => qeaMonitor.step(QeaMonitor.CLOSE, pid, fd)
        case Event('Spawn, parent : Int, child : Int) => qeaMonitor.step(QeaMonitor.SPAWN, parent, child)
        case Event('Exit, pid : Int) => qeaMonitor.step(QeaMonitor.EXIT, pid)
      }
      assert(verdict);
    }
    val stop_qea = System.nanoTime()
    println("CSP_E Elapsed: " + (stop_qea - start_qea) / scala.math.pow(10, 9) + "s")

  }
}
