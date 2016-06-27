/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class Interrupt(p0 : Process, as0 : Set[Symbol], q0 : Process) extends Process{
  private val p = p0
  private val as = as0
  private val q = q0

  override def acceptPrim(e : AbsEvent) : Process = {
    val next = p << e
    if (as contains e.alphabet) {
      if (next.isFailure) {
        Failure
      } else {
        q
      }
    } else {
      interrupt(next, as, q)
    }
  }

  override def canTerminatePrim = {
    p.canTerminatePrim
  }

  override def toString = p.toString + "|" + as.toString + ">" + q.toString

  def canEqual(other: Any): Boolean = other.isInstanceOf[Interrupt]

  override def equals(other: Any): Boolean = other match {
    case that: Interrupt =>
      (that canEqual this) &&
        p == that.p &&
        as == that.as &&
        q == that.q
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(p, as, q)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
