/*
 *
 *  * Copyright (c) 2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

import jp.go.aist.cspe.CSPE._

object SKIP extends Process {
  override def acceptPrim(e : AbsEvent): ProcessSet = processSet(List.empty)

  override def canTerminate = true

  override def toString = "SKIP"
  override def equals(other: Any) = other match {
    case that: AnyRef => this eq that
    case _ => false
  }
  override def hashCode() = "SKIP".hashCode()

}
