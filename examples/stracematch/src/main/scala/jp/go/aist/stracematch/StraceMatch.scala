/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */


import jp.go.aist.cspe.CSPE._
import jp.go.aist.cspe._
import jp.go.aist.stracematch._

import scala.io.StdIn

object StraceMatch {

  def parseLine(line : String): Array[String] = {
    val s1 = line split "[:,=()]+" map(_.trim) filter(_ != "")
    val s2 = s1 map((s: String) =>
      if (s matches "\".*\"" ){
        Array(s)
      } else {
        s split " "
      })
    s2 flatten
  }

  def h2i(s : String) = Integer.parseInt(s.substring(2), 16)

  def StringsToEvent(args : Array[String]) : Event = {
    val hds = args.head.split("/")
    val pid = hds(0).toInt
    val threadId = h2i(hds(1))
    val sym = Symbol(args(1))
    Event(sym, pid, threadId, args.tail.tail)
  }

  def hex(i : Any) = {
    val n = i.toString.toInt
    "0x" + n.toHexString
  }

  def nextProcess(pid : Int, fds : Map[Int, FdData], closedFds : Set[Int]) : Process = {
  //  println(pid + " : " + fds.keySet + "," + closedFds)
    if (fds.keySet subsetOf Set(0, 1, 2)) {
      SKIP <+> unixProcess(pid, fds, closedFds)
    } else {
      unixProcess(pid, fds, closedFds)
    }
  }

  def copyFds(fds : Map[Int, FdData]) : Map[Int, FdData] =
    fds.mapValues(_.dup)

  def closeAfterExecv(fds : Map[Int, FdData], closedFds : Set[Int]) : (Map[Int, FdData], Set[Int]) =
    (fds filterKeys (! fds(_).isCloseOnExec), closedFds ++ (fds filterKeys (fds(_).isCloseOnExec)).keySet)

  def unixProcess(pid : Int, fds : Map[Int, FdData], closedFds : Set[Int]) : Process = ?? {
    //Create file descriptors
    case Event('creat, _) => Failure //creat is obsolue
    case Event('open | 'open_nocancel, `pid`, tid: Int, Array( _, oflag: String, _, s: String, _)) =>
  //    println("open ! " + s)
      val fd = s.toInt
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        val fdData = new FdData(tid, (h2i(oflag) & 0x1000000) == 1) //O_CLOEXEC
        nextProcess(pid, fds + (fd -> fdData), closedFds - fd)
      }
    case Event('shm_open, `pid`, tid: Int, Array( _, _, s: String, _)) =>
      val fd = s.toInt
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        val fdData = new FdData(tid, true) //O_CLOEXEC
        nextProcess(pid, fds + (fd -> fdData), closedFds - fd)
      }
    case Event('shm_open, `pid`, tid: Int, Array( _, _, _, s: String, _)) =>
      val fd = s.toInt
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        val fdData = new FdData(tid, true) //O_CLOEXEC
        nextProcess(pid, fds + (fd -> fdData), closedFds - fd)
      }
    case Event('socket | 'accept, `pid`, tid: Int, Array(_, _, _, s: String, _)) =>
      val fd = s.toInt
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        val fdData = new FdData(tid, false)
        nextProcess(pid, fds + (fd -> fdData), closedFds - fd)
      }

    case Event('socketpair | 'pipe | 'opendir, `pid`, _, _) => nextProcess(pid, fds, Set.empty)
    case Event('dup, `pid`, tid: Int, Array(_, s: String, _)) =>
      val fd = s.toInt
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        nextProcess(pid, fds + (fd -> new FdData(tid, false)), closedFds - fd)
      }
    case Event('dup2, `pid`, tid: Int, Array(_, s: String, _)) =>
      val fd = h2i(s)
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        nextProcess(pid, fds + (fd -> new FdData(tid, false)), closedFds - fd)
      }
    case Event('fcntl, `pid`, tid: Int, Array(_, c: String, s: String, _)) if h2i(c) == 0 => //F_DUPFD=0
      val fd = s.toInt
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        nextProcess(pid, fds + (fd -> new FdData(tid, false)), closedFds - fd)
      }
    case Event('fcntl, `pid`, tid: Int, Array(_, c: String, s: String, _)) if h2i(c) == 67 => //F_DUPFD_CLOEXEC=67
      val fd = s.toInt
      if (fd == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        nextProcess(pid, fds + (fd -> new FdData(tid, true)), closedFds - fd)
      }
    //Closing file descriptor
    case Event('close | 'close_nocancel, `pid`, tid : Int, Array(s: String, _, _)) =>
      val fd = h2i(s)
      if (closedFds contains fd) {Failure} else {
        (fds get fd) flatMap (_.openThread) match {
        case None => nextProcess(pid, fds - fd, closedFds + fd)
        case Some(tid1) =>
          if (tid1 == tid) {nextProcess(pid, fds - fd, closedFds + fd)} else Failure
      }
    }
    //Changing properties
    case Event('fcntl, `pid`, tid: Int, Array(s: String, c: String, _, _)) if h2i(c) == 4 => //F_SETFD
      val fd = h2i(s)
      val flag = h2i(c)
      nextProcess(pid, fds + (fd -> new FdData(tid, (flag & 0x1000000) == 1)), closedFds)

    //manipulating processes
    case Event('fork, `pid`, _, Array(s : String, _)) =>
      val pid1 = s.toInt
      if (pid1 == -1 || pid1 == 0){
        nextProcess(pid, fds, closedFds)
      } else {
        nextProcess(pid, fds, closedFds) ||| nextProcess(pid1, copyFds(fds), Set.empty)
      }
    case Event('execve, `pid`, _, Array(_, _, _, s : String, _)) =>
      if (s.toInt == -1) {
        nextProcess(pid, fds, closedFds)
      } else {
        val (fds1, closedFds1) = closeAfterExecv(fds, closedFds)
        nextProcess(pid, fds1, closedFds1)
      }
    //Otherwise
    case Event(_, `pid`, _, _) =>
      nextProcess(pid, fds, closedFds)
  }

  def system : Process = <+> {
    case Event(_, pid : Int, _, _) =>
      val fdData = new FdData(None, false)
      unixProcess(pid, Map(0 -> fdData, 1 -> fdData, 2 -> fdData), Set.empty)
  }

  def main (as : Array[String]) {
    var line = StdIn.readLine()
    var monitor = processSet(List(system))
    var line_num = 0

    while (line != null) {
      val args : Array[String] = parseLine(line)
    //  println(args.mkString(","))
    //  println(monitor)
      line_num += 1
      val event = try {Some(StringsToEvent(args))} catch { case e : Exception => None }
      event match {
        case Some(event) =>
          monitor = monitor << event
        case None => }
      if (monitor.isFailure) { println(line_num.toString + ": " + line); sys.exit(-1)}

      line = StdIn.readLine()
    }
    if (monitor.canTerminate) println("Success!") else {
      println(monitor)
    }
  }
}
