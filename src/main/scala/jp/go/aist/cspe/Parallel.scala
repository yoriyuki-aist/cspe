/*
 * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */

package jp.go.aist.cspe
import scala.collection
import scala.collection.immutable.{HashBag=>Bag}
import jp.go.aist.cspe.CSPE._


private[cspe] class Parallel(processes0 : Bag[Process], as0 : Set[Symbol]) extends Process {

  private[cspe] val processes = processes0

  private[cspe] val as = as0
  // FIXME : naive implementation
  private def allCombination(b : Iterable[Set[Process]]) : Set[Bag[Process]] = {
    if (b isEmpty) Set(Bag.empty) else {
      val ps = b.head
      val rest = b.tail
      ps flatMap (p => allCombination(rest) map (_ + p))
    }
  }

  private def collapseBag(s : Bag[Process]) : Bag[Process] = {
    if (s contains Failure) Bag(Failure) else s
  }

  override def acceptPrim(e: AbsEvent): ProcessSet = {
    implicit val config = Bag.configuration.compact[Process]
    if (as.contains(e.alphabet)) {
      val pnext = processes map (_.accept(e).processes)
      val ps = allCombination(pnext) map collapseBag
      processSet(ps map (parallel(_, as)))
    } else {
      val nextPs: Set[Process] =
        processes flatMap { p: Process =>
          val rest: Bag[Process] = processes - p
          val nextp: Set[Process] = p.accept(e).processes
          nextp map ((q: Process) => parallel(rest + q, as))
        } toSet;
      processSet(nextPs)
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
