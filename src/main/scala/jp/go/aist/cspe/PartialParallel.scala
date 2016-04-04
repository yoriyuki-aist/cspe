/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class PartialParallel(p1 : Process, as : Set[Symbol]) {
  def || (p2 : Process) = parallel(List(p1, p2), as)
}
