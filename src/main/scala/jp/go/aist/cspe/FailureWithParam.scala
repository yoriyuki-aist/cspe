/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._
/**
  * Created by yoriyuki on 2016/06/27.
  */
class FailureWithParam(arg: Any) extends FailureClass {
  private val param = arg

  override def toString() = "Failure" + param.toString()
  override def handle(handler: PartialFunction[Any, Process]) = {
    handler(param)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[FailureWithParam]

  override def equals(other: Any): Boolean = other match {
    case that: FailureWithParam =>
      (that canEqual this) &&
        param == that.param
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(param)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
