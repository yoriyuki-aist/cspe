/*
 * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class Choice(ps0 : Set[Process]) extends Process {
  // used for verification
  private val ps = ps0
  override def acceptPrim(e: AbsEvent): ProcessSet = {
    val qs = ps.flatMap(_.accept(e).processes)
    processSet(qs)
   }

  override def canTerminate : Boolean = {
    ps.exists(_.canTerminate)
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
