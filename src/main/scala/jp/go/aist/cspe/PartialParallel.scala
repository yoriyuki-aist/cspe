/*
 * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */

package jp.go.aist.cspe
import scala.collection
import scala.collection.immutable.{HashBag=>Bag}
import jp.go.aist.cspe.CSPE._

private[cspe] class PartialParallel(p1 : Process, as : Set[Symbol]) {
  implicit val config = Bag.configuration.compact[Process]
  def || (p2 : Process) = parallel(Bag(p1, p2), as)
}
