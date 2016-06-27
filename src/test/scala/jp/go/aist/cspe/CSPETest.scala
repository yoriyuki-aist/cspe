/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

import jp.go.aist.cspe.CSPE._
import jp.go.aist.cspe._

import scala.collection.mutable

object CSPETest {
  def main(args: Array[String]) {

    val p = rec0 (x => Event('open, "a.txt") ->:
      Event('close, "a.txt") ->: x <+>
      Event('open, "b.txt") ->: Event('close, "b.txt") ->: STOP)

    val p1 = p <<
      Event('open, "a.txt") <<
      Event('close, "a.txt") <<
      Event('open, "a.txt") <<
      Event('close, "a.txt") <<
      Event('open, "b.txt") <<
      Event('close, "b.txt")
    assert(!p1.isFailure) // should OK

    val p2 = p << Event('close, "b.txt")
    assert(p2.isFailure)

    val p3 = p << Event('open, "a.txt") << Event('close, "b.txt")
    assert(p3.isFailure)

    val q = rec0 (x => Event('open, "a.txt") ->:
      Event('close, "a.txt") ->: x)

    val q1 = q <<
      Event('open, "a.txt") <<
      Event('close, "a.txt") <<
      Event('open, "a.txt") <<
      Event('close, "a.txt")
    assert(!q1.isFailure)

    val s = Event('open, "a.txt") ->:
      Event('close, "a.txt") ->: STOP <+>
      Event('open, "b.txt") ->: Event('close, "b.txt") ->: STOP

    val s1 = s <<
      Event('open, "a.txt") <<
      Event('close, "b.txt")
    assert(s1.isFailure)

    val parametric_process =
      rec0 (parametric_process => ?? {case Event('open, x) => Event('close, x) ->: parametric_process})

    val result = parametric_process <<
      Event('open, "a.txt") <<
       Event('close, "a.txt") <<
       Event('open, "a.txt") <<
       Event('close, "a.txt") <<
       Event('open, "b.txt") <<
       Event('close, "b.txt")
    assert(!result.isFailure)

    val more_param_p = rec0 (x => ?? {case Event('open, file) =>
      Event('close, file) ->: x} <+>
      Event('open, "a.txt") ->: Event('close, "b.txt") ->: STOP)

    val more_param_p_result = more_param_p <<
      Event('open, "a.txt") <<
      Event('close, "a.txt") <<
      Event('open, "a.txt") <<
      Event('close, "a.txt") <<
      Event('open, "a.txt") <<
      Event('close, "b.txt")
    assert(!more_param_p_result.isFailure)

    val par : Process = (Event('a) ->: STOP) || Set.empty || (Event('b) ->: STOP)

    val par_result = par << Event('a) << Event('b)
    assert(!par_result.isFailure)

    val par_result2 = par << Event('b) << Event('a)
    assert(!par_result2.isFailure)

    val par2 = (Event('a) ->: STOP) || Set('a) || (Event('b) ->: Event('a) ->: STOP)

    val par2_result = par2 << Event('b) << Event('a) << Event('a)
    assert(par2_result.isFailure)

    val par2_result2 = par2 << Event('a)
    assert(par2_result2.isFailure)

    val intl2 : Process = ?? {case Event('a, _) => STOP} ||| ?? {case Event(_, _) => Failure}

    val intl2_result = intl2 << Event('a, 1)
    assert(!intl2_result.isFailure)
    val intl2_result2 = intl2 << Event('b, 1)
    assert(intl2_result2.isFailure)

    val intl : Process = (Event('a) ->: STOP) ||| (Event('b) ->: STOP)

    val intl_result = intl << Event('a) << Event('b)
    assert(!intl_result.isFailure)
    val intl_result2 = intl << Event('b) << Event('a)
    assert(!intl_result2.isFailure)


    val seq = Event('a) ->: SKIP $ Event('b) ->: SKIP

    val seq_ret = seq << Event('a) << Event('b)
    println(seq_ret)
    assert(!seq_ret.isFailure)

    val inter = (Event('a) ->: Event('b) ->: Event('c) ->: SKIP) | Set('b) |> (Event('d) ->: SKIP)

    val inter_result = inter << Event('a) << Event('b) << Event('d)
    assert(inter_result == SKIP)

    val inter_result2 = inter << Event('a) << Event('b) << Event('c)
    assert(inter_result2.isFailure)

//    val hide = (Event('a) ->: Event('b) ->: Event('c) ->: STOP) \ Event('b)
//    val hide_result = hide << Event('a) << Event('c)
//    assert(! hide_result.isFailure)

    val auction0 =
      rec1 [Int]((a: Int => Process) => (max : Int) =>
        ?? { case Event('bid, p : String) => ? (p.toInt > max) {a(p.toInt)}} : Process) _

    val auction_ret = auction0(0) << Event('bid, "4") << Event('bid, "10") << Event('bid, "20")
    assert(!auction_ret.isFailure)

    val auction_invalid = auction_ret << Event('bid, "5")
    assert(auction_invalid.isFailure)

    def auction(max : Int) : Process = ?? {
      case Event('bid, p : Int) =>
          ? (p > max) {auction(p)}
    }

    val auction_ret1 = auction(0) << Event('bid, 4) << Event('bid, 10) << Event('bid, 20)
    assert(!auction_ret1.isFailure)

    val auction_invalid1 = auction_ret1 << Event('bid, 5)
    assert(auction_invalid1.isFailure)

    val lock_order =
      rec0 (p =>
        ?? {case Event('acquire, t1, a) =>
          ?? {case Event('release, "t1", "a") => p
          case Event('acquire, "t1", "b") =>
          ?? {case Event('acquire, t2, "b") =>
          Event('release, "t2", "b") ->: p}}})

    val lock_order_valid = lock_order <<
      Event('acquire, "t1", "a") <<
      Event('release, "t1", "a") //<<
     // Event('acquire, "t2", "b") <<
    // Event('release, "t2", "b")
    assert(!lock_order_valid.isFailure)

    val lock_order_invalid = lock_order <<
      Event('acquire, "t1", "a") <<
      Event('release, "t1", "b") <<
      Event('acquire, "t2", "b") <<
      Event('release, "t2", "a")
    assert(lock_order_invalid.isFailure)

    val lock_order_klaus =
      process (p =>
        ??? {
          case Event('acquire, t1, a) =>
    p |||
              ??? {
                case Event('release, `t1`, `a`) => STOP
                case Event('acquire, `t1`, b) if a != b =>
                  ??? {
                    case Event('acquire, t2, `b`) =>
                      ??? {
                        case Event('release, `t2`, `b`) => STOP
                        case Event('acquire, `t2`, `a`) => assert(false); Failure
                      }
                  }
              }
        }
      )

    val lock_order_klaus_valid =
      try {lock_order_klaus <<
      Event('acquire, "t1", "a") <<
      Event('release, "t1", "a") <<
     Event('acquire, "t2", "b") <<
     Event('release, "t2", "b"); true}
      catch{case e => false}
    assert(lock_order_klaus_valid)

    val lock_order_klaus_invalid =
      try {lock_order_klaus <<
      Event('acquire, "t1", "a") <<
      Event('acquire, "t1", "b") <<
      Event('acquire, "t2", "b") <<
      Event('acquire, "t2", "a"); false}
      catch {case e => true}
    assert(lock_order_klaus_invalid)

    def hex(i : Any) = {
      val n = i.toString.toInt
      "0x" + n.toHexString
    }

    def openCloseSimpl : Process = ?? {
      case Event('open) =>
        (openCloseSimpl <+> SKIP) |||
          ?? { case Event('close) => SKIP}
      case Event('close) => Failure
      case _ => openCloseSimpl <+> SKIP
    }

    println(openCloseSimpl)

    //    assert(! openCloseRet.terminate.isFailure)

    val openCloseRet1 =
      openCloseSimpl <<
        Event('open)

    println(openCloseRet1)
 //   println(openCloseRet1.terminate)
//    assert(openCloseRet2.terminate.isFailure)

    val openCloseRet2 =
      openCloseRet1 <<
        Event('close)

    println(openCloseRet2)

    val openCloseRet3 =
      openCloseRet2 <<
        Event('open)

    println(openCloseRet3)

    val openCloseRet4 =
      openCloseRet3 <<
        Event('close)

    println(openCloseRet4)

    val openCloseRet5 =
      openCloseRet4 <<
        Event('open)

    println(openCloseRet5)

    val openCloseRet6 =
      openCloseRet5 <<
        Event('close)

    println(openCloseRet6)

    val openCloseRet7 =
      openCloseRet6 <<
        Event('open)


    println(openCloseRet7)

    val openCloseRet8 =
      openCloseRet7 <<
        Event('close)

    println(openCloseRet8)

    val openCloseRet9 =
      openCloseRet8 <<
        Event('any)

    val openCloseRet10 =
      openCloseRet9 <<
        Event('any)

    val openCloseRet11 =
      openCloseRet10 <<
        Event('any)

    assert(openCloseRet11.canTerminate)

    val openCloseFailure : Process =
      openCloseSimpl << Event('close)

    assert(openCloseFailure.isFailure)

    val openCloseNoFailure =
      process (v1 = p =>
        ?? {
          case Event('open) =>
            (p <+> SKIP) ||| ?? { case Event('close) => SKIP }
          case e if e != Event('close) => p <+> SKIP
        }
      )

    val openCloseFailure2 : Process=
      openCloseNoFailure << Event('close)

    println(openCloseFailure2)
    assert(openCloseFailure2.isFailure)

    // Examples in Havelund and Reger paper

    def readOnly(file: String) : Process = ?? {
      case Event('read, `file`) => readOnly(file)
      case Event('close, `file`) => SKIP
    }

    def writeOnly(file: String, n: Int) : Process =
      ?? {
        case Event('write, `file`, b: Int)
          if n + b <= 16000000 =>
          writeOnly(file, n + b)
        case Event('close, `file`) => SKIP
      }

    def fileUsage : Process = ?? {
      case Event('open, file: String, m: Symbol, size: Int) =>
        var n: Int = size
        (fileUsage <+> SKIP) |||
          (m match {
            case 'readonly =>
              readOnly(file)
            case 'writeonly =>
              writeOnly(file, size)
          })
    }

    assert (!{
      fileUsage |= List(Event('open, "A", 'readonly,  0),
      Event('open, "B", 'writeonly, 15999900),
      Event('write, "B", 100),
      Event('read, "A"),
      Event('write, "B", 100),
      Event('close, "B"))})


    // Rover examples


    /// leader election

    def roverCoordination = SKIP <+> listener(Set.empty)

    def listener(knownRovers : Set[Int]) : Process =
      ?? {
        case e@Event(_, r1: Int, r2: Int) =>
          if ((knownRovers & Set(r1, r2)) == Set.empty) {
            leader(r1, knownRovers + r2, Set.empty, Set.empty) << e <+>
              leader(r2, knownRovers + r1, Set.empty, Set.empty) << e <+> listener(knownRovers + r1 + r2)
          } else if ((knownRovers & Set(r1, r2)) == Set(r1, r2)) {
            listener(knownRovers)
          } else if (knownRovers contains r1) {
            leader(r2, knownRovers, Set.empty, Set.empty) << e <+> listener(knownRovers + r2)
          } else {
            leader(r1, knownRovers, Set.empty, Set.empty) << e <+> listener(knownRovers + r1)
          }
      }

    def leader(r : Int, unPingedRovers : Set[Int], waitRovers : Set[Int], pingedRovers : Set[Int]) : Process = {
        ?? {
          case Event('ping, `r`, r1: Int) =>
            leader(r, unPingedRovers - r1, waitRovers + r1, pingedRovers + r1)
          case Event('ack, r1 : Int, `r`) =>
            val waitRovers1 = waitRovers - r1
            if (unPingedRovers == Set.empty && waitRovers1 == Set.empty) {
              SKIP <+> leader(r, unPingedRovers, waitRovers1, pingedRovers)
            } else {
              leader(r, unPingedRovers, waitRovers1, pingedRovers)
            }
          case Event(_, r1: Int, r2: Int) =>
            val rs : Set[Int] = if (pingedRovers.contains(r1) || r1 == r) unPingedRovers else {unPingedRovers + r1}
            val rs1 : Set[Int] = if (pingedRovers.contains(r2) || r2 == r) rs else {rs + r2}
            leader(r, rs1, waitRovers, pingedRovers)
        }
    }

   assert(roverCoordination |= List(Event('ping, 1, 2), Event('ping, 1, 3), Event('ping, 2, 3), Event('ack, 2, 1), Event('ack, 3, 1)))

    assert(!(roverCoordination |= List(Event('ping, 1, 2), Event('ping, 1, 3), Event('ping, 2, 3), Event('ack, 2, 1), Event('ack, 3, 2))))


    val success = process (p => ?? {case _ => p})

    def pinged(r1 : Int, r2 : Int) =
    Event('ping, r1, r2) ->:
    ?? {
      case Event('ack, `r2`, `r1`) => SKIP
    }

    def ignore(r : Int) : Process =
      ?? {
        case Event('ping, from, to) if from != r => ignore(r)
        case Event('ack, from, to) if to != r => ignore(r)
      } <+> SKIP

    val leader1 = pinged(1, 2) ||| pinged(1, 3) ||| ignore(1)
    val leader2 = pinged(2, 1) ||| pinged(2, 3) ||| ignore(2)
    val leader3 = pinged(3, 1) ||| pinged(3, 2) ||| ignore(3)

    val roverCoordinate = leader1 <+> leader2 <+> leader3

    assert(roverCoordinate |= List(Event('ping, 1, 2), Event('ping, 1, 3), Event('ping, 2, 3), Event('ack, 2, 1), Event('ack, 3, 1)))

    /// Command nesting

    def commandWait(command : Symbol) : Process = process {
      p => ?? {
        case Event('suc, `command`) => SKIP
        case Event('cmd, c : Symbol) => commandWait(c) $ Event('suc, `command`) ->: SKIP
      }
    }

    val commandNesting = ?? {
      case Event('cmd, c : Symbol) => commandWait(c)
    }

    assert(commandNesting |= List(Event('cmd, 'c1), Event('cmd, 'c2), Event('cmd, 'c3), Event('suc, 'c3), Event('suc, 'c2), Event('suc, 'c1)))

    /// Resource management

    val assignedResources = mutable.HashMap.empty[Symbol, Symbol]
    val conflictResources = mutable.HashMap.empty[Symbol, Set[Symbol]]

    def addResource(t : Symbol, r : Symbol) = assignedResources += (r -> t)

    def freeResource(r : Symbol) = assignedResources -= r

    def isGranted(r : Symbol) = assignedResources contains r

    def hasResource(t : Symbol, r : Symbol) = isGranted(r) && assignedResources(r) == t

    def addConflict(r1 : Symbol, r2 : Symbol) = {
      val currentConflict = if (conflictResources contains r1) {conflictResources(r1)} else Set.empty[Symbol]
      conflictResources += (r1 -> (currentConflict + r2))
      val currentConflict2 = if (conflictResources contains r2) {conflictResources(r2)} else Set.empty[Symbol]
      conflictResources += (r2 -> (currentConflict + r1))
    }

    def isConflict(r1 : Symbol, r2 : Symbol) = (conflictResources contains r1) && (conflictResources(r1) contains r2)

    def hasConflict(r : Symbol) = (assignedResources keySet) exists (isConflict(r, _))

    def clear() = {
      assignedResources.clear
      conflictResources.clear
    }


    def resourceCycle : Process = ?? {
          case Event('request, t: Symbol, r: Symbol) if !hasResource(t, r) =>
            resourceCycle ||| requested(t, r)
          case Event('conflict, r1 : Symbol, r2 : Symbol) =>
            addConflict(r1, r2)
            resourceCycle
    } <+> SKIP


    def requested(t: Symbol, r: Symbol) = ?? {
      case Event('deny, `t`, `r`) => SKIP
      case Event('grant, `t`, `r`) =>
        if (isGranted(r) || hasConflict(r)) {Failure} else {
          addResource(t, r)
          granted(t, r)
        }
    }

    def granted(t: Symbol, r : Symbol) : Process = ?? {
      case Event('cancel, `t`, `r`) => SKIP
      case Event('rescind, `t`, `r`) => granted(t, r)
    }

    clear()
    assert(resourceCycle |= List(Event('request, 't1, 'r1), Event('request, 't2, 'r1), Event('deny, 't2, 'r1),
      Event('grant, 't1, 'r1), Event('rescind, 't1, 'r1), Event('cancel, 't1, 'r1)))

    clear()
    assert(! (resourceCycle |= List(Event('request, 't1, 'r1), Event('request, 't2, 'r1), Event('deny, 't2, 'r1),
      Event('grant, 't1, 'r1), Event('rescind, 't1, 'r1), Event('request, 't1, 'r1))))

    //// Mutual Exclusion
    clear()
    assert(! (resourceCycle |= List(Event('request, 't1, 'r1), Event('request, 't2, 'r1), Event('grant, 't2, 'r1),
      Event('grant, 't1, 'r1), Event('rescind, 't1, 'r1), Event('cancel, 't1, 'r1), Event('cancel, 't2, 'r1))))

    //// Conflict
    clear()
    assert(resourceCycle |= List(Event('request, 't1, 'r1), Event('request, 't2, 'r2), Event('grant, 't2, 'r2),
      Event('grant, 't1, 'r1), Event('cancel, 't1, 'r1), Event('cancel, 't2, 'r2)))

    clear()
    assert(!(resourceCycle |= List(Event('conflict, 'r1, 'r2), Event('request, 't1, 'r1), Event('request, 't2, 'r1), Event('grant, 't2, 'r2),
      Event('grant, 't1, 'r1), Event('cancel, 't1, 'r1), Event('cancel, 't2, 'r2))))

    ///Concurrency
    ////Lock nesting

    val locks = mutable.HashSet.empty[Symbol]
    val knownThread = mutable.HashSet.empty[Symbol]

    def lockNesting : Process = ?? {
        case Event('begin, t: Symbol) if !(knownThread contains t) =>
          knownThread += t
          lockNesting ||| methodCall(t)
      } <+> SKIP

    def methodCall(t : Symbol) : Process = ?? {
        case Event('begin, `t`) => methodCall(t) $ methodCall(t)
        case Event('end, `t`) => SKIP
        case Event('lock, `t`, l : Symbol) if !(locks contains l) =>
          locks += l
          lock(t, l) $ methodCall(t)
    }

    def lock(t: Symbol, l : Symbol) : Process = ?? {
      case Event('unlock, `t`, `l`) =>
        locks -= l
        SKIP
      case Event('begin, `t`) => methodCall(t) $ lock(t, l)
    }

    assert(lockNesting |= List(Event('begin, 't1), Event('lock, 't1, 'l1), Event('begin, 't1), Event('end, 't1), Event('unlock, 't1, 'l1), Event('end, 't1)))

    locks.clear()
    knownThread.clear()

    assert(!(lockNesting |= List(Event('begin, 't1), Event('lock, 't1, 'l1), Event('begin, 't1), Event('unlock, 't1, 'l1), Event('end, 't1), Event('end, 't1))))

    //// Lock ordering

    val Order = mutable.Map.empty[Symbol, Set[Symbol]]

    def lockOrder : Process = ?? {
      case Event('lock, t : Symbol, l : Symbol) => lockOrder || Set('lock) || locked(t, l)
    }

    def locked(t : Symbol, l1 : Symbol) : Process = ??? {
      case Event('lock, `t`, l2 : Symbol) =>
        if (!((Order contains l2) && (Order(l2) contains l1))) {
          val oldSet: Set[Symbol] = if (Order contains l1) {
            Order(l1)
          } else {
            Set.empty
          }
          Order += (l1 -> (oldSet + l2))
          locked(t, l1)
        } else {
          Failure
        }
    }

    println(lockOrder << Event('lock, 't1, 'l1) << Event('lock, 't2, 'l1) << Event('lock, 't1, 'l2) << Event('lock, 't2, 'l2))

    assert(lockOrder |~ List(Event('lock, 't1, 'l1), Event('lock, 't2, 'l1), Event('lock, 't1, 'l2), Event('lock, 't2, 'l2)))

    Order.clear()

    assert(!(lockOrder |~ List(Event('lock, 't1, 'l1), Event('lock, 't2, 'l2), Event('lock, 't1, 'l2), Event('lock, 't2, 'l1))))

    println("Success!")
  }
}
