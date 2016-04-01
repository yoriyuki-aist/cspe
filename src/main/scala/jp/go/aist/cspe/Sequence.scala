/*
 *
 *  * Copyright (c) 2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._



private[cspe] class Sequence(ps0 : List[Process]) extends Process {

  private def normalize (ps : List[Process]) : List[Process] = ps match {
    case Nil => Nil
    case Failure :: ps1 => List(Failure)
    case SKIP :: ps1 => normalize(ps1)
    case p :: ps1 => p :: normalize(ps1)
  }

  private val ps = normalize(ps0)
  override def acceptPrim(e: AbsEvent): ProcessSet = ps match {
    case Nil => processSet(List.empty)
    case p :: ps =>
      val next = p << e
      val nextp0 = next.processes map ((p: Process) => {
        if (p == SKIP) {
          if (ps.size == 1) ps.last else sequence(ps)
        } else if (p == Failure) { Failure
        } else {
          sequence(p :: ps)
        }
      })
      val nextp = if (p.canTerminate) {nextp0 ++ sequence(ps).acceptPrim(e).processes} else nextp0
      processSet(nextp filter (!_.isFailure))
  }

  override def canTerminate = ps.forall(_.canTerminate)

  override def toString = "$" + ps.toString()

  def canEqual(other: Any): Boolean = other.isInstanceOf[Sequence]

  override def equals(other: Any): Boolean = other match {
    case that: Sequence =>
      (that canEqual this) &&
        ps == that.ps
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(ps)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}