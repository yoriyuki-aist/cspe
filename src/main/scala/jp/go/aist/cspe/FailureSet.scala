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
class FailureSet(fs: List[FailureClass]) extends FailureClass {
  private val fails = fs

  override def handle(handler: PartialFunction[Any, Process]): Process = {
    choice(fails map (f => if (handler.isDefinedAt(f)) handler(f) else Failure))
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[FailureSet]

  override def equals(other: Any): Boolean = other match {
    case that: FailureSet =>
      (that canEqual this) &&
        fails == that.fails
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(fails)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
