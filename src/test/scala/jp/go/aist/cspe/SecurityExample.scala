/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import javax.security.auth.login.CredentialException

import jp.go.aist.cspe.CSPE._
import qea.util.ArrayUtil


/**
  * Created by yoriyuki on 2016/08/08.
  */
object SecurityExample extends ExampleTrait {
  var procNum = 1
  var credentialNum = 1

  def addMap[T, X](m: Map[T, Set[X]], k: T, v: X) = m updated (k, (m getOrElse (k, Set())) + v)

  def removeMap[T, X](m: Map[T, Set[X]], k: T, v: X) ={
    val n = m(k)
    if (! m.isEmpty) {
      m updated (k, n - v)
    } else {
      m - k
    }
  }

  def removeMapFromSetElem[T, X](m: Map[T, Set[X]], v: X) : Map[T, Set[X]] = {
    for ((k, s) <- m) yield (k, s - v)
  }

  def genEventStream(n: Int) = {

    def client(pid: Int, credentials: Set[Int], obtained: Map[Int, Set[Int]], children: List[Stream[AbsEvent]]) : Stream[AbsEvent] =
      TraceFactory.choice (Array(
        {
          val child = procNum
          procNum += 1
          val obtained1 = for ((c, s) <- obtained) yield (c, s + child)
          Event('Spawn, pid, child) #:: client(pid, credentials, obtained1,
              client(child, credentials, Map(), List()) ::children)
        },
        {
          val credential = credentialNum
          credentialNum += 1
          Event('Atomic, List(Event('Req, pid),
            Event('MakeResAvailable),
            Event('BecomeResAvailable, credential),
            Event('Granted, pid, credential))) #::
            client(pid, credentials + credential, addMap(obtained, credential, pid), children)

        })
        ++ {if (!children.isEmpty){Array(
        {
          val n = TraceFactory.random(children.length)
          val head = children(n)(0)
          head match {
            case Event('Exit, child: Int) => head #::
              client(pid, credentials, removeMapFromSetElem(obtained, child),
                children.take(n) ++ children.drop(n+1))
            case _ =>
              val children1 = children updated (n, children(n).drop(1))
              head #:: client(pid, credentials, obtained, children1)
          }
        }
      )} else {
        Array[Stream[AbsEvent]]()}
      } ++ {
          val owned = obtained.keySet.filter(c => obtained.getOrElse(c, Set()) == Set(pid))
          (if (!owned.isEmpty) {
            Array({
              val credential = TraceFactory.randomChoice(owned)
              Event('Atomic, List(Event('Release, pid, credential),
                Event('ReleaseRes, credential))) #::
                client(pid, credentials - credential, removeMap(obtained, credential, pid), children)
            })
          } else {
            Array[Stream[AbsEvent]]()
          })} ++ {
            if (obtained.isEmpty && pid != 0 && children.isEmpty) {
              Array(Event('Exit, pid) #:: Stream[AbsEvent]())
            } else {
              Array[Stream[AbsEvent]]()
            }
        }
        )


    val ret = TraceFactory.removeAtomic(client(0, Set(), Map(), List()).take(n)).toList
    val count = ret.count(_ match {case Event('Spawn, _, _) => true; case _ => false})
    println("Processes: " + count)
    ret
  }

  def authServer : Process = ?? {
    case Event('Req, pid: Int)  =>
      Event('MakeResAvailable) ->:
        ?? { case Event('BecomeResAvailable, credential) =>
            Event('Granted, pid, credential) ->: authServer
        }
    case Event('Release, pid: Int, credential: Int) =>
      Event('ReleaseRes, credential) ->: authServer
  }

  def resource(credentials: Set[Int]) : Process = ?? {
    case Event('MakeResAvailable) =>
      ??{ case Event('BecomeResAvailable, credential : Int) =>
      resource(credentials + credential)}
    case Event('ReleaseRes, credential: Int) if credentials(credential) =>
        resource(credentials - credential)
    case Event('Access, pid: Int, credential: Int) if credentials(credential) =>
        resource(credentials)
  }

  def client(pid: Int, credentials: Set[Int], obtained:Map[Int, Set[Int]], children: Set[Int]): Process = ?? {
    case Event('Spawn, `pid`, child: Int) =>
      val obtained1 = for ((c, s) <- obtained) yield (c, s + child)
      client(pid, credentials, obtained1, children + child) ||Set('Exit) || client(child, credentials ++ obtained.keys, Map.empty, Set.empty)
    case Event('Req, `pid`) =>
      ??{case Event('Granted, `pid`, credential: Int) =>
        client(pid, credentials + credential, addMap(obtained, credential, pid), children)}
    case Event('Release, `pid`, credential: Int) if obtained.getOrElse(credential, Set()) == Set(pid) =>
      client(pid, credentials - credential, removeMap(obtained, credential, pid), children)
    case Event('Access, `pid`, credential: Int) if credentials(credential) =>
      client(pid, credentials, obtained, children)
    case Event('Exit, `pid`) if obtained.isEmpty && children.isEmpty => SKIP
    case Event('Exit, child: Int) if children(child) =>
      client(pid, credentials, removeMapFromSetElem(obtained, child), children - child)
    case Event('Exit, p) if p != pid => client(pid, credentials, obtained, children)
  }

  def server = (authServer || Set('MakeResAvailable, 'BecomeResAvailable, 'ReleaseRes) || resource(Set()))

  def uniqProcess(pidSet : Set[Int]) : Process = ?? {
    case Event('Spawn, _, child_pid : Int) if ! pidSet(child_pid) => uniqProcess(pidSet + child_pid)
  }

  def system = server || Set('Req, 'Granted, 'Access, 'Release) || (client(0, Set(), Map(), Set()) || Set('Spawn) || uniqProcess(Set(0)))

  def createCSPEModel() : Process = system
  def debugCSPEModel() = {
    val monitors = new ProcessSet(List(system))

    assert(!monitors.isFailure)
    assert(!(monitors |~ List(Event('Access, 4))))
    assert(monitors |~ List(Event('Req, 0), Event('MakeResAvailable), Event('BecomeResAvailable, 1),
      Event('Granted, 0, 1), Event('Access, 0, 1), Event('Spawn, 0, 1), Event('Access, 1, 1), Event('Exit, 1),
      Event('Release, 0, 1), Event('ReleaseRes, 1)).take(8))


    assert(new ProcessSet(List(server)) |~ List(
      Event('Req, 0),
      Event('MakeResAvailable),
      Event('BecomeResAvailable, 1),
      Event('Granted, 0, 1),
      Event('Release, 0, 1),
      Event('ReleaseRes, 1)))

    //      println((monitors /: List(Event('Req,0),
    //      Event('MakeResAvailable),
    //      Event('BecomeResAvailable, 1),
    //      Event('Granted,0, 1),
    //      Event('Spawn,0, 2),
    //      Event('Req,0),
    //      Event('Req,2),
    //      Event('MakeResAvailable),
    //      Event('BecomeResAvailable,3),
    //      Event('Granted,0, 3),
    //      Event('Spawn,0, 5),
    //      Event('MakeResAvailable),
    //      Event('BecomeResAvailable,4),
    //      Event('Access,0, 1),
    //      Event('Access,0, 3),
    //      Event('Access,0, 3),
    //      Event('Granted,2, 4),
    //      Event('Req,0),
    //      Event('Access,2, 4),
    //      Event('Release,2, 4),
    //      Event('MakeResAvailable),
    //      Event('ReleaseRes,4)).take(22)) (_ << _))

    assert((monitors /: List(
        Event('Spawn,0, 4),
        Event('Spawn,4, 6),
      Event('Exit, 6),
      Event('Exit, 0)
    )) (_ << _) .isFailure)

  }



  def createQeaModel(): QeaMonitor = new StubQeaMonitor()
  def debugQeaModel() = {}

  def symbolMap = Map('Req -> 1, 'MakeResAvailable -> 2, 'Granted -> 3, 'Release -> 4, 'BecomeResAvailable -> 5,
  'ReleaseRes -> 6, 'Access -> 7, 'Spawn ->8, 'Exit -> 9)
}

