/*
 * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */


package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

import scalacache._
import guava._
import memoization._

private[cspe] class Rec1[X] (f0 : (X =>Process) => (X => Process), args0 : X, id0 : Int) extends Process {
  private val f = f0
  private val args = args0
  val id = id0

  lazy val that = {
    f(rec1(f))(args)
  }
  override def acceptPrim(e : AbsEvent) : ProcessSet =
    that << e

  override def canTerminate = that canTerminate

  def canEqual(other: Any): Boolean = other.isInstanceOf[Rec1[X]]

  override def equals(other: Any): Boolean = other match {
    case that: Rec1[X] =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    id
  }

}

