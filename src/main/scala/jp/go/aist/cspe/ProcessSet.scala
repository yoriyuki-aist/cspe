/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

/**
  * Copyright (c) 2014, 2015, 2016 National Institute of Advanced Industrial Science and Technology (AIST)
  * All rights reserved.
  */


import jp.go.aist.cspe.CSPE._

class ProcessSet (ps: List[Process]) {
  val processes : List[Process] = ps.filterNot (_.isFailure)

  def accept(e: AbsEvent): ProcessSet = {
    val rs : List[Process] = processes.flatMap(_.accept(e).processes)
    processSet(rs)
  }

  def canTerminate = {
    processes.exists(_.canTerminate)
  }

  def <<(e : AbsEvent) = accept(e)

  def |=(s: Traversable[AbsEvent]): Boolean = {
    val rs = (processes /: s)((ps, e) => ps.flatMap(_.accept(e).processes))
    processSet(rs).canTerminate
  }

  def |~(s: Traversable[AbsEvent]): Boolean = {
    val rs = (processes /: s)((ps, e) => ps.flatMap(_.accept(e).processes))
    !processSet(rs).isFailure
  }


  def isFailure = processes.isEmpty

  override def toString = "{" + processes.toString + "}"

  def canEqual(other: Any): Boolean = other.isInstanceOf[ProcessSet]

  override def equals(other: Any): Boolean = other match {
    case that: ProcessSet =>
      (that canEqual this) &&
        processes == that.processes
    case _ => false
  }

  override def hashCode(): Int = {
    processes.hashCode()
  }
}