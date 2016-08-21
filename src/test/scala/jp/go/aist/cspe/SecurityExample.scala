/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._


/**
  * Created by yoriyuki on 2016/08/08.
  */
object SecurityExample extends ExampleTrait {
  var procNum = 1
  var credentialNum = 1

  def genEventStream(n: Int) = {

    def eventStream(pid: Int, credentials: Set[Int], used: Set[Int], obtained: Set[Int]): Stream[AbsEvent] =
      TraceFactory.choice (Array(
        {
          val child = procNum
          procNum += 1
          Event('Spawn, pid, child) #::
            TraceFactory.interleaving(eventStream(pid, credentials, obtained, obtained),
              eventStream(child, credentials, Set.empty, Set.empty)) #:::
            eventStream(pid, credentials, Set.empty, obtained)
        }, {
          val credential = credentialNum
          credentialNum += 1
          Event('Atomic, List(Event('Req, pid),
            Event('MakeResAvailable),
            Event('BecomeResAvailable, credential),
            Event('Granted, pid, credential))) #::
            eventStream(pid, credentials + credential, used, obtained + credential)

        }) ++
        (if (!credentials.isEmpty) {
          Array({
              val credential = TraceFactory.randomChoice(credentials)
              Event('Access, pid, credential) #:: eventStream(pid, credentials, used, obtained)
            })
        }
        else {
          Array[Stream[AbsEvent]]()
        }) ++
        (if (!obtained.isEmpty && used.isEmpty) {
          Array({
            val credentail = TraceFactory.randomChoice(obtained)
            Event('Atomic, List(Event('Release, pid, credentail),
              Event('ReleaseRes, credentail))) #::
              eventStream(pid, credentials - credentail, Set.empty, obtained - credentail)
          })
        } else {
          Array[Stream[AbsEvent]]()
        }) ++
        (if (! (pid == 0) && used.isEmpty) {
          Array(Stream.empty)
        } else {
          Array[Stream[AbsEvent]]()
        })
      )
    val ret = TraceFactory.removeAtomic(eventStream(0, Set.empty, Set.empty, Set.empty)).take(n).toList
    println("Processes: " + procNum + " credentials: " + credentialNum)
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

  def client(pid: Int, credentials: Set[Int]): Process = ?? {
    case Event('Spawn, `pid`, child_pid: Int) => client(pid, credentials) ||| client(child_pid, credentials)
    case Event('Req, `pid`) =>
      ??{case Event('Granted, `pid`, credential: Int) => client(pid, credentials + credential) $
        Event('Release, `pid`, credential) ->: client(pid, credentials)}
    case Event('Access, `pid`, credential: Int) if credentials(credential) => client(pid, credentials)
  } <+> SKIP

  def server = (authServer || Set('MakeResAvailable, 'BecomeResAvailable, 'ReleaseRes) || resource(Set()))

  def uniqProcess(pidSet : Set[Int]) : Process = ?? {
    case Event('Spawn, _, child_pid : Int) if ! pidSet(child_pid) => uniqProcess(pidSet + child_pid)
  }

  def system = server || Set('Req, 'Granted, 'Access, 'Release) || (client(0, Set()) || Set('Spawn) || uniqProcess(Set()))

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
    //
  }



  def createQeaModel(): QeaMonitor = new StubQeaMonitor()
  def debugQeaModel() = {}

  def symbolMap = Map('Req -> 1, 'MakeResAvailable -> 2, 'Granted -> 3, 'Release -> 4, 'BecomeResAvailable -> 5,
  'ReleaseRes -> 6, 'Access -> 7, 'Spawn ->8, 'Exit -> 9)
}

