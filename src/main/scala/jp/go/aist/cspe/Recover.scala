/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

/**
  * Created by yoriyuki on 2016/06/27.
  */
private[cspe] class Recover(p0 : Process, errorHandler0 : PartialFunction[Any, Process]) extends Process {
  private val p = p0
  private val errorHandler = errorHandler0

  private[cspe] override def acceptPrim(e : AbsEvent): Process = {
    p << e match {
      case p : FailureClass => p handle errorHandler
      case p : _ => p recover errorHandler
    }
  }

  override def canTerminatePrim = p canTerminate

  def canEqual(other: Any): Boolean = other.isInstanceOf[Recover]

  override def equals(other: Any): Boolean = other match {
    case that: Recover =>
      (that canEqual this) &&
        p == that.p &&
        errorHandler == that.errorHandler
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(p, errorHandler)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
