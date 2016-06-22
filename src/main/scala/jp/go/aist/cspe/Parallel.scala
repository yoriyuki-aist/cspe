/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class Parallel(processes0 : List[Process], as0 : Set[Symbol]) extends Process {

  private[cspe] val processes = processes0

  private[cspe] val as = as0

  private def collapse(s : List[Process]) : List[Process] = {
    if (s contains Failure) List(Failure) else s
  }

  private def triplePartitions[X] : List[X] => List[Tuple3[List[X], X ,List[X]]] = {
    case Nil => Nil
    case e :: y => (Nil, e, y) :: (triplePartitions(y) map (t => (e :: t._1, t._2, t._3)))
  }

  override def acceptPrim(e: AbsEvent): Process = {
    if (as.contains(e.alphabet)) new Parallel(processes map (_.accept(e)), as0)
    else {
      val nextPs = {
        triplePartitions(processes) map { t =>
          val nextP = t._2.acceptPrim(e)
          parallel(t._1 ++ (nextP :: t._3), as)
        }
      }
      choice(nextPs)
    }
  }

  override def canTerminate = {
    processes.forall(_.canTerminate)
  }

  override def toString = "||" + as.toString + " " + processes.toString

  def canEqual(other: Any): Boolean = other.isInstanceOf[Parallel]

  override def equals(other: Any): Boolean = other match {
    case that: Parallel =>
      (that canEqual this) &&
        processes == that.processes &&
        as == that.as
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(processes, as)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
