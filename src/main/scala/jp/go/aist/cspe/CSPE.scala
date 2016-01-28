/*
 * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 * All rights reserved.
 */

package jp.go.aist.cspe
import scala.collection
import scala.collection.immutable.Stream.Empty
import scala.collection.immutable.{HashBag=>Bag}
import scalacache._
import guava._
import memoization._

object CSPE {
  implicit val config = Bag.configuration.compact[Process]

  private var objId = 0

  def rec0(f: Process => Process): Process = {
    objId += 1
    new Rec0(f, objId)
  }

  // Klaus's naming
  def process: (Process => Process) => Process = rec0

  def rec1[X](f: (X => Process) => (X => Process))(args: X): Process = {
    objId += 1
    new Rec1(f, args, objId)
  }

  def ?(g: Boolean): Process => Process = p => if (g) p else Failure

  def ??(f: PartialFunction[AbsEvent, Process]): Process = {
    objId += 1
    new ParamPrefix(f, objId)
  }

  def ???(f: PartialFunction[AbsEvent, Process]): Process = {
    objId += 1
    new ParamPrefixRelaxed(f, objId) /// Relaxed semantics
  }

  def choice(ps: Set[Process]): Process =
    new Choice(ps)

  def parallel(ps: Bag[Process], as: Set[Symbol]): Process = {
     if (ps isEmpty) SKIP else {
        if (ps contains Failure) Failure else new Parallel(ps, as)
    }
  }

  def ||(as : Set[Symbol], f : (AbsEvent => Process)) :Process =
    ?? {
      case e => parallel(Bag(choice(f(e) << e processes), choice(||(as, f) << e processes)), as)
    }


  def |||(f : (AbsEvent => Process)) :Process =
    ?? {
      case e => parallel(Bag(choice(f(e) << e processes), choice(|||(f) << e processes)), Set.empty)
    }


  def <+> (f : (AbsEvent => Process)) : Process =
    ?? {
      case e => choice(f(e) << e processes)
    }


  def sequence(ps: List[Process]): Process =
    new Sequence(ps)


  def interrupt(p: Process, es: Set[Symbol], q: Process): Process =
    new Interrupt(p, es, q)

  
  def processSet(ps : Set[Process]) : ProcessSet = new ProcessSet(ps)

}