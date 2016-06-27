/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

private[cspe] class Rec0 (f0 : Process => Process, id0 : Int) extends Process {
  private val f = f0
  lazy val that = f(this)
  val id = id0
  override def acceptPrim(e : AbsEvent) : Process = that.accept(e)

  override def canTerminatePrim = that.canTerminatePrim

  def canEqual(other: Any): Boolean = other.isInstanceOf[Rec0]

  override def equals(other: Any): Boolean = other match {
    case that: Rec0 =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    id
  }
}
