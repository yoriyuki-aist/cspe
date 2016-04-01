/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

object STOP extends Process{

  // used for verification
  override def acceptPrim(e : AbsEvent): ProcessSet = processSet(List.empty)

  override def canTerminate = false

  override def toString = "STOP"
  override def equals(other: Any) = other match {
    case that: AnyRef => this eq that
    case _ => false
  }
  override def hashCode() = "STOP".hashCode()

}
