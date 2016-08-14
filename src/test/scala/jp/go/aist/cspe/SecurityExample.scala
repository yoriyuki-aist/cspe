/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import jp.go.aist.cspe.CSPE._
import sun.security.krb5.Credentials

/**
  * Created by yoriyuki on 2016/08/08.
  */
object SecurityExample extends ExampleTrait {

  def authServer(credentials : Set[Int]) : Process = ?? {
    case Event('Req, credential: Int) if ! credentials(credential) =>
      authServer(credentials + credential) |||
        Event('MakeResAvailable, credential) ->: Event('BecomeResAvailable, credential) ->:
        Event('Granted, credential) ->: SKIP
    case Event('Release, credential: Int) if credentials(credential) =>
      authServer(credentials - credential) ||| Event('ReleaseRes, credential) ->: SKIP
  }

  def resource(credentials_asked: Set[Int], credentials_granted: Set[Int]) : Process = ?? {
    case Event('MakeResAvailable, credential: Int) => resource(credentials_asked + credential, credentials_granted)
    case Event('BecomeResAvailable, credential : Int) if credentials_asked(credential) =>
      resource(credentials_asked - credential, credentials_granted + credential)
    case Event('ReleaseRes, credential: Int) if credentials_granted(credential) =>
        resource(credentials_asked, credentials_granted - credential)
    case Event('Access, credential: Int) if credentials_granted(credential) =>
        resource(credentials_asked, credentials_granted)
  }

  def client(pid: Int, credentials: Set[Int]): Process = ?? {
    case Event('Spawn, `pid`, child_pid: Int) => client(pid, credentials) ||| client(child_pid, credentials)
    case Event('Req, credential: Int) =>
      Event('Granted, credential) ->: client(pid, credentials + credential)
    case Event('Access, credential: Int) if credentials(credential) => client(pid, credentials)
    case Event('Release, credential: Int) if credentials(credential) => client(pid, credentials - credential)
  }

  def server = (authServer(Set()) || Set('MakeResAvailable, 'BecomeResAvailable, 'ReleaseRes) || resource(Set(), Set()))

  def system = server || Set('Req, 'Granted, 'Access, 'Release) || client(0, Set())

  def createCSPEModel() : Process = system
}

