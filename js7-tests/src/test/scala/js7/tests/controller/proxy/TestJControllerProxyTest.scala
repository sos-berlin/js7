package js7.tests.controller.proxy

import js7.base.annotation.javaApi

/** Duplicates TestControllerProxy, to include a valid test logger in classpath. */
@javaApi
object TestJControllerProxyTest:
  def main(args: Array[String]) =
    TestJControllerProxy.main(args)
    println("Terminating")
