/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

import jp.go.aist.cspe._
/**
  * Created by yoriyuki on 2016/03/28.
  */

object TraceFactory {
  var prng = new scala.util.Random()

  def randomChoice[T](s: Set[T]): T = {
    val n = prng.nextInt(s.size)
    val it = s.iterator.drop(n)
    it.next()
  }

  def random(n: Int) : Int = prng.nextInt(n)

  def set_seed(seed : Int) =
    prng = new scala.util.Random(seed)

  val skip = Stream.empty

  def choice (as : Array[Stream[AbsEvent]]) : Stream[AbsEvent] = {
    val n = as.length
    val i = prng.nextInt(n)
    as(i)
  }

  def interleaving(a : Stream[AbsEvent], b : Stream[AbsEvent]) : Stream[AbsEvent] =
    if (b.isEmpty) {a} else {
      if (a.isEmpty) {b} else {
        if (prng.nextBoolean()) {
          val head : AbsEvent = a(0)
          val rest : Stream[AbsEvent] = a.drop(1)
          head #:: interleaving(rest, b)
        } else {
          val head = b(0)
          val rest = b.drop(1)
          head #:: interleaving(a, rest)
        }
      }
    }

  def removeAtomic(a : Stream[AbsEvent]) : Stream[AbsEvent] = a match {
    case Stream.Empty => Stream.Empty
    case Event('Atomic, events: List[AbsEvent]) #:: rest => events ++: removeAtomic(rest)
    case e #:: rest => e #:: removeAtomic(rest)
  }
}
