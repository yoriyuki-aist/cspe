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
private[cspe] abstract class FailureClass extends Process {
  sealed override def isFailure = true
  sealed override def acceptPrim(e : AbsEvent) = Failure
  sealed override def canTerminatePrim = false

  abstract def handle(handler : PartialFunction[Any, Process]) : Process
}
