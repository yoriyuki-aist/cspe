/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
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
  override def acceptPrim(e: AbsEvent): Process = ps match {
    case Nil => Failure
    case p :: ps =>
      val next = p << e
      if (next.isFailure) {
        Failure
      } else if (next.canTerminate) {
        sequence(ps)
      } else {
        sequence(next :: ps)
      }
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