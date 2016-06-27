/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class Choice(ps0: List[Process]) extends Process {
  // used for verification
  private val ps = ps0 flatMap (_.choiceProcesses)

  private[cspe] override def choiceProcesses = ps

  private[cspe] override def acceptPrim(e: AbsEvent): Process = {
    val qs = ps.map(_.<<(e))
    choice(qs)
   }

  override def canTerminatePrim : Boolean = {
    ps.exists(_.canTerminatePrim)
  }

  override def toString = "<+>" + ps.toString

  def canEqual(other: Any): Boolean = other.isInstanceOf[Choice]

  override def equals(other: Any): Boolean = other match {
    case that: Choice =>
      (that canEqual this) &&
        ps == that.ps
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(ps)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
