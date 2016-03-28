/*
 * Copyright (c) 2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */

package jp.go.aist.cspe

import jp.go.aist.cspe._
/**
  * Created by yoriyuki on 2016/03/28.
  */

object TraceFactory {
  var prng = new scala.util.Random()

  def set_seed(seed : Int) =
    prng = new scala.util.Random(seed)

  val skip = Stream.empty

  def choice (a : Stream[AbsEvent], b : Stream[AbsEvent]) : Stream[AbsEvent] =
    if (prng.nextBoolean()) a else b

  def interleving(a : Stream[AbsEvent], b : Stream[AbsEvent]) : Stream[AbsEvent] =
    if (prng.nextBoolean()) {
      val head : AbsEvent = a(0)
      val rest : Stream[AbsEvent] = a.drop(1)
      head #:: interleving(rest, b)
    } else {
      val head = b(0)
      val rest = b.drop(1)
      head #:: interleving(a, rest)
    }
}
