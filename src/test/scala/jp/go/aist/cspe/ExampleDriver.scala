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
  private[cspe] def run(): Unit ={
    example.debugCSPEModel()
    example.debugQeaModel()
    val iteration = 0

    val system = example.createCSPEModel()

    val max_trace = example.genEventStream(iteration)

    var qeaTime: List[Double] = List()
    var cspeTime: List[Double] = List()

    val qeaMonitor = example.createQeaModel()
    val start_qea = System.nanoTime()
    var trace = max_trace
    for (i <- 1 to iteration/10000) {
      val chunk = trace.take(10000)
      trace = trace.drop(10000)

      for (e <- chunk) {
        val verdict = e match {
          case Event(s: Symbol, a: Int) => qeaMonitor.step(example.symbolMap(s), a)
          case Event(s: Symbol, a: Int, b: Int) => qeaMonitor.step(example.symbolMap(s), a, b)
        }
        assert(verdict)
      }
      val rap_qea = System.nanoTime()
      qeaTime = qeaTime :+ (rap_qea - start_qea) / scala.math.pow(10, 9)
    }

    trace = max_trace
    val start = System.nanoTime()
    var cspe_monitors: ProcessSet = new ProcessSet(List(system))

    for (i <- 1 to iteration/10000) {
      val chunk = trace.take(10000)
      trace = trace.drop(10000)

      for (e <- chunk) {
        cspe_monitors = cspe_monitors << e
      }
      val rap_cspe = System.nanoTime()
      cspeTime = cspeTime :+ (rap_cspe - start) / scala.math.pow(10, 9)
    }
    assert(!cspe_monitors.isFailure)

    println("," + Calendar.getInstance().getTime() + ",")
    val rs = qeaTime zip cspeTime
    var count = 10000
    for (ret <- rs) {
      println(count + "," + ret._1 + "," + ret._2)
      count = count + 10000
    }
  }
}
