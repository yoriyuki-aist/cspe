/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

/**
  * Created by yoriyuki on 2016/08/10.
  */
private[cspe] trait ExampleTrait {
  private[cspe] def createCSPEModel(): Process
  private[cspe] def createQeaModel(): QeaMonitor
  private[cspe] def debugCSPEModel(): Unit
  private[cspe] def debugQeaModel(): Unit
  private[cspe] def genEventStream(num : Int) : List[AbsEvent]
  private[cspe] def symbolMap: Map[Symbol, Int]
}
