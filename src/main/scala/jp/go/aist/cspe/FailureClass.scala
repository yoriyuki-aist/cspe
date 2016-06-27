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
  final override def acceptPrim(e : AbsEvent) = Failure
  final override def canTerminatePrim = false
  final override def toFailure = Some(this)

  def handle(handler : PartialFunction[Any, Process]) : Process
}
