/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class Prefix(e00 : AbsEvent, p0 : Process) extends Process {
  private val e0 = e00
  private val p = p0
  // used for verification
  override def acceptPrim(e: AbsEvent): ProcessSet =
    if (e == e0) processSet(List(p)) else processSet(List.empty)

  override def toString = e0.toString + "->" + p.toString

  override def canTerminate = false

  def canEqual(other: Any): Boolean = other.isInstanceOf[Prefix]

  override def equals(other: Any): Boolean = other match {
    case that: Prefix =>
      (that canEqual this) &&
        e0 == that.e0 &&
        p == that.p
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(e0, p)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}


