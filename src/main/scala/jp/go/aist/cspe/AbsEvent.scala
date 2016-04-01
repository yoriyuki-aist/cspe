/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

abstract class AbsEvent {def alphabet : Symbol}
case class Event(s : Symbol, es : Any*) extends AbsEvent {override def alphabet = s}