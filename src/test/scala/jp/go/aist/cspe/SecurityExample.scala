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

  def genEventStream(n: Int) = {
    var procNum = 1
    var credentialNum = 1

    def eventStream(pid: Int, credentials: Set[Int], obtained: Set[Int]): Stream[AbsEvent] =
      TraceFactory.choice (Array(
        {
          val child = procNum
          procNum += 1
          Event('Spawn, pid, procNum) #::
            TraceFactory.interleving(eventStream(pid, credentials, obtained), eventStream(child, credentials, Set.empty))
        }, {
          val credential = credentialNum
          credentialNum += 1
          Event('Req, pid, credential) #::
            Event('MakeResAvailable, credential) #::
            Event('BecomeResAvailable, credential) #::
            Event('Granted, credential) #::
            eventStream(pid, credentials + credentialNum, obtained + credential)
        }) ++
        (if (!credentials.isEmpty) {
          Array({
              val credentail = credentials.toIndexedSeq.apply(TraceFactory.random(credentials.size))
              Event('Access, pid, credentail) #:: eventStream(pid, credentials, obtained)
            })
        }
        else {
          Array(TraceFactory.skip)
        }) ++
        (if (!obtained.isEmpty) {
          Array({
            val credentail = credentials.toIndexedSeq.apply(TraceFactory.random(obtained.size))
            Event('Release, pid, credentail) #::
              Event('ReleaseRes, credentail) #::
              eventStream(pid, credentials - credentail, obtained - credentail)
          })
        } else {
          Array(TraceFactory.skip)
        })
      )
    val ret = eventStream(0, Set.empty, Set.empty).take(n).toList
    println("Processes: " + procNum + " credentials: " + credentialNum)
    ret
  }

  def authServer(credentials : Set[(Int, Int)]) : Process = ?? {
    case Event('Req, pid: Int, credential: Int) if ! credentials(pid, credential) =>
      authServer(credentials + ((pid, credential))) |||
        Event('MakeResAvailable, credential) ->: Event('BecomeResAvailable, credential) ->:
        Event('Granted, credential) ->: SKIP
    case Event('Release, pid: Int, credential: Int) if credentials(pid, credential) =>
      authServer(credentials - ((pid, credential))) ||| Event('ReleaseRes, credential) ->: SKIP
  }

  def resource(credentials_asked: Set[Int], credentials_granted: Set[Int]) : Process = ?? {
    case Event('MakeResAvailable, credential: Int) => resource(credentials_asked + credential, credentials_granted)
    case Event('BecomeResAvailable, credential : Int) if credentials_asked(credential) =>
      resource(credentials_asked - credential, credentials_granted + credential)
    case Event('ReleaseRes, credential: Int) if credentials_granted(credential) =>
        resource(credentials_asked, credentials_granted - credential)
    case Event('Access, pid: Int, credential: Int) if credentials_granted(credential) =>
        resource(credentials_asked, credentials_granted)
  }

  def client(pid: Int, credentials: Set[Int]): Process = ?? {
    case Event('Spawn, `pid`, child_pid: Int) => client(pid, credentials) ||| client(child_pid, credentials)
    case Event('Req, `pid`, credential: Int) =>
      Event('Granted, credential) ->: client(pid, credentials + credential) $
        Event('Release, `pid`, credential) ->: client(pid, credentials)
    case Event('Access, `pid`, credential: Int) if credentials(credential) => client(pid, credentials)
  } <+> SKIP

  def server = (authServer(Set()) || Set('MakeResAvailable, 'BecomeResAvailable, 'ReleaseRes) || resource(Set(), Set()))

  def uniqProcess(pidSet : Set[Int]) : Process = ?? {
    case Event('Spawn, _, child_pid : Int) if ! pidSet(child_pid) => uniqProcess(pidSet + child_pid)
  }

  def system = server || Set('Req, 'Granted, 'Access, 'Release) || (client(0, Set()) || Set('Spawn) || uniqProcess(Set()))

  def createCSPEModel() : Process = system
  def debugCSPEModel() = {
    val monitors = new ProcessSet(List(system))
    val serverMonitors = new ProcessSet(List(server))

    assert(! monitors.isFailure)
    assert(! (monitors |~ List(Event('Access, 4))))
    assert(monitors |~  List(Event('Req, 0, 1), Event('MakeResAvailable, 1), Event('BecomeResAvailable, 1),
      Event('Granted, 1), Event('Access, 0, 1), Event('Spawn, 0, 1), Event('Access, 1, 1),
      Event('Release, 0, 1), Event('ReleaseRes, 1)))
  }

  def createQeaModel(): QeaMonitor = new StubQeaMonitor()
  def debugQeaModel() = {}

  def symbolMap = Map('Req -> 1, 'MakeResAvailable -> 2, 'Granted -> 3, 'Release -> 4, 'BecomeResAvailable -> 5,
  'ReleaseRes -> 6, 'Access -> 7, 'Spawn ->8, 'Exit -> 9)
}

