/*
 * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._

private[cspe] class ParamPrefix(f : PartialFunction[AbsEvent, Process], id0 : Int) extends Process {
    // used for verification
    val id = id0
    override def acceptPrim(e: AbsEvent): ProcessSet =
      if (f.isDefinedAt(e)) processSet(Set(f(e))) else processSet(Set.empty)

    override def canTerminate = false

  def canEqual(other: Any): Boolean = other.isInstanceOf[ParamPrefix]

  override def equals(other: Any): Boolean = other match {
    case that: ParamPrefix =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    id
  }

}
