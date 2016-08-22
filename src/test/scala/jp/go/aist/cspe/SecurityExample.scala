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

  def genEventStream(n: Int) = {

    def client(pid: Int, credentials: Set[Int], obtained: Set[Int]) : Stream[AbsEvent] =
      TraceFactory.choice (Array(
        eventStream(pid, credentials) #::: client(pid, credentials, obtained),
      {
        val credential = credentialNum
        credentialNum += 1
        Event('Atomic, List(Event('Req, pid),
          Event('MakeResAvailable),
          Event('BecomeResAvailable, credential),
          Event('Granted, pid, credential))) #:: {
          client(pid, credentials + credential, obtained + credential)
        }
      }) ++
        (if (!obtained.isEmpty) {
          Array({
            val credentail = TraceFactory.randomChoice(obtained)
            Event('Atomic, List(Event('Release, pid, credentail),
              Event('ReleaseRes, credentail))) #::
              client(pid, credentials - credentail, obtained - credentail)
          })
        } else {
          Array[Stream[AbsEvent]]()
        }) ++
        (if (pid != 0 && obtained.isEmpty) {
          Array(Event('Exit, pid) #:: Stream[AbsEvent]())
        } else Array[Stream[AbsEvent]]())
      )

    def eventStream(pid: Int, credentials: Set[Int]): Stream[AbsEvent] =
      TraceFactory.choice (Array(
        {
          val child = procNum
          procNum += 1
          Event('Spawn, pid, child) #:: {
            TraceFactory.interleaving(eventStream(pid, credentials),
              client(child, credentials, Set.empty)) #:::
              eventStream(pid, credentials)
          }
        }) ++
        (if (!credentials.isEmpty) {
          Array({
              val credential = TraceFactory.randomChoice(credentials)
              Event('Access, pid, credential) #:: eventStream(pid, credentials)
            })
        }
        else {
          Array[Stream[AbsEvent]]()
        }))

    val ret = TraceFactory.removeAtomic(client(0, Set.empty, Set.empty)).take(n).toList
    val count = ret.count(_ match {case Event('Spawn, _) => true; case _ => false})
    println("Processes: " + count)
    ret
  }

  def authServer : Process = ?? {
    case Event('Req, pid: Int)  =>
      authServer ||| Event('MakeResAvailable) ->:
        ?? { case Event('BecomeResAvailable, credential) =>
            Event('Granted, pid, credential) ->: SKIP
        }
    case Event('Release, pid: Int, credential: Int) =>
      authServer ||| Event('ReleaseRes, credential) ->: SKIP
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

  def credentialStore(pid: Int, credentials: Set[Int]): Process = ??{
    case Event('Granted, _, credential: Int) => credentialStore(pid, credentials + credential)
    case Event('Release, _, credential: Int) if credentials(credential) => credentialStore(pid, credentials - credential)
    case Event('Exit, `pid`) if credentials.isEmpty => SKIP
    case Event('Exit, _) => credentialStore(pid, credentials)
  }

  def client(pid: Int, credentials: Set[Int]): Process = ?? {
    case Event('Spawn, `pid`, child_pid: Int) =>
      client(pid, credentials) |||
        (client(child_pid, credentials) || Set('Granted, 'Release, 'Exit) || credentialStore(child_pid, Set.empty))
    case Event('Req, `pid`) =>
      ??{case Event('Granted, `pid`, credential: Int) => client(pid, credentials + credential)}
    case Event('Release, `pid`, credential: Int) if credentials(credential) => client(pid, credentials - credential)
    case Event('Access, `pid`, credential: Int) if credentials(credential) => client(pid, credentials)
    case Event('Exit, `pid`) => SKIP
  }

  def server = (authServer || Set('MakeResAvailable, 'BecomeResAvailable, 'ReleaseRes) || resource(Set()))

  def uniqProcess(pidSet : Set[Int]) : Process = ?? {
    case Event('Spawn, _, child_pid : Int) if ! pidSet(child_pid) => uniqProcess(pidSet + child_pid)
  }

  def system = server || Set('Req, 'Granted, 'Access, 'Release) || (client(0, Set()) || Set('Spawn) || uniqProcess(Set(0)))

  def createCSPEModel() : Process = system
  def debugCSPEModel() = {
    val monitors = new ProcessSet(List(system))

    assert(!monitors.isFailure)
    assert(!(monitors |~ List(Event('Access, 4))))
    assert(monitors |~ List(Event('Req, 0), Event('MakeResAvailable), Event('BecomeResAvailable, 1),
      Event('Granted, 0, 1), Event('Access, 0, 1), Event('Spawn, 0, 1), Event('Access, 1, 1),
      Event('Release, 0, 1), Event('ReleaseRes, 1)).take(9))


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

    assert(!(monitors /: List(
        Event('Spawn,0, 4),
        Event('Spawn,4, 6),
        Event('Exit,6)).take(3)
    ) (_ << _) .isFailure)

  }



  def createQeaModel(): QeaMonitor = new StubQeaMonitor()
  def debugQeaModel() = {}

  def symbolMap = Map('Req -> 1, 'MakeResAvailable -> 2, 'Granted -> 3, 'Release -> 4, 'BecomeResAvailable -> 5,
  'ReleaseRes -> 6, 'Access -> 7, 'Spawn ->8, 'Exit -> 9)
}

