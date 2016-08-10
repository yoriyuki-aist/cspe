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
  private[cspe] abstract def createCSPEModel: Unit => Process
  private[cspe] abstract def createQEAModel: Unit => QeaMonitor
  private[cspe] abstract def debugCSPEModel: Unit => Unit
  private[cspe] abstract def debugQeaModel:  Unit => Unit
  private[cspe] abstract def genEventStream: Int => List[AbsEvent]
  private[cspe] abstract def symbolMap: Map[Symbol, Int]
}
