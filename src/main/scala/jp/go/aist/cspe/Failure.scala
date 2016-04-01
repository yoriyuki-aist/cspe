/*
 *
 *  * Copyright (c) 2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

object Failure extends Process {
  override def isFailure = true
  override def acceptPrim(e : AbsEvent) = processSet(List(this))
  override def canTerminate = false
  override def toString = "Failure"

  override def equals(other: Any) = other match {
    case that: AnyRef => this eq that
    case _ => false
  }
  override def hashCode() = "Failure".hashCode()
  }
