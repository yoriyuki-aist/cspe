/*
 *
 *  * Copyright (c) 2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe._
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

  def process(pid : Int, openFiles : Set[Int]) : Stream[AbsEvent] =
    genEventStream(pid, openFiles) ++ Event('Exit, pid) #:: skip

  def genEventStream(pid : Int, openFiles : Set[Int]) : Stream[AbsEvent] =
    choice(
      Array(
        Event('Access, pid, random(openFiles)) #:: genEventStream(pid, openFiles),
        {
          val fd = 1 + openFiles.max
          Event('Open, pid, fd) #:: genEventStream(pid, openFiles + fd) ++
            Event('Close, pid, fd) #:: genEventStream(pid, openFiles)
        },
        {
          val child_pid = 1 + max_pid
          max_pid += 1
          Event('Spawn, pid, child_pid) #::
            interleving(genEventStream(pid, openFiles), process(child_pid, openFiles))
        }
      ) ++
        (if (pid == 0) Array.empty[Stream[AbsEvent]] else Array(skip))
    )

  def main(args: Array[String]) {
    print(process(0, Set(0, 1, 2)) take 1000 foreach println)
  }
}
