/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class PartialInterrupt(p : Process, as : Set[Symbol]) {
  def |> (q : Process) = interrupt(p, as, q)
}
