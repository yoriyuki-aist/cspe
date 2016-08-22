/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe
import java.util.{Calendar, Date}

/**
  * Created by yoriyuki on 2016/08/10.
  */
private[cspe] class ExampleDriver(example: ExampleTrait) {
private[cspe] def run(do_qea: Boolean, do_cspe: Boolean): Unit ={
    example.debugCSPEModel()
    example.debugQeaModel()

    val iteration = 30000
    val chunkSize = 10000

    val system = example.createCSPEModel()

    val max_trace = example.genEventStream(iteration)

    System.gc()

    var qeaTime: List[Double] = List()
    var cspeTime: List[Double] = List()

    if (do_qea) {
      val qeaMonitor = example.createQeaModel()
      val start_qea = System.nanoTime()
      var trace = max_trace
      for (i <- 1 to iteration / chunkSize) {
        val chunk = trace.take(chunkSize)
        trace = trace.drop(chunkSize)

        for (e <- chunk) {
          val verdict = e match {
            case Event(s: Symbol, a: Int) => qeaMonitor.step(example.symbolMap(s), a)
            case Event(s: Symbol, a: Int, b: Int) => qeaMonitor.step(example.symbolMap(s), a, b)
          }
          if (!verdict) println(e)
          assert(verdict)
        }
        val rap_qea = System.nanoTime()
        qeaTime = qeaTime :+ (rap_qea - start_qea) / scala.math.pow(10, 9)
      }
    }

    if (do_cspe) {
      var trace = max_trace
      val start = System.nanoTime()
      var cspe_monitors: ProcessSet = new ProcessSet(List(system))

      for (i <- 1 to iteration / chunkSize) {
        val chunk = trace.take(chunkSize)
        trace = trace.drop(chunkSize)

        for (e <- chunk) {
          cspe_monitors = cspe_monitors << e
          //if (cspe_monitors.isFailure) {println(e)}
//          println(e)
          assert(!cspe_monitors.isFailure)
        }
        val rap_cspe = System.nanoTime()
        cspeTime = cspeTime :+ (rap_cspe - start) / scala.math.pow(10, 9)
      }
    }


    println("," + Calendar.getInstance().getTime() + ",")
    if (do_qea && do_cspe) {
      val rs = qeaTime zip cspeTime
      var count = chunkSize
      for (ret <- rs) {
        println(count + "," + ret._1 + "," + ret._2)
        count = count + chunkSize
      }
    } else {
      val rs = if (do_qea) {qeaTime} else {cspeTime}
      var count = chunkSize
      for (ret <- rs) {
        println(count + "," + ret)
        count = count + chunkSize
      }
    }
  }
}
