/*
 * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */

package jp.go.aist.stracematch

class FdData(t : Option[Int], b : Boolean) {
  val openThread = t
  val isCloseOnExec = b

  def this(t : Int, b : Boolean) = this(Option(t), b)
  def this(b : Boolean) =  this(None, b)

  def dup = new FdData(isCloseOnExec)
}
