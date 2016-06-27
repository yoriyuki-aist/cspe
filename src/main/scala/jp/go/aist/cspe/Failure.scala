/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] object Failure extends FailureClass{
  override def toString = "Failure"

  override def handle(handler: PartialFunction[Any, Process]): Process = {
    handler(())
  }

  override def equals(other: Any) = other match {
    case that: AnyRef => this eq that
    case _ => false
  }
  override def hashCode() = "Failure".hashCode()
  }
