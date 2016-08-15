/*
 *
 *  * Copyright (c) 2014-2016. National Institute of Advanced Industrial Science and Technology (AIST)
 *  * All rights reserved.
 *
 */

package jp.go.aist.cspe

/**
  * Created by yoriyuki on 2016/08/10.
  */
object ExampleRunner {
  def main(args: Array[String]): Unit ={
    val driver1 = new ExampleDriver(MotivatingExample)
    val driver2 = new ExampleDriver(SecurityExample)

    driver1.run()
    driver2.run()
  }
}
