package js7.tests.master.proxy

import js7.base.annotation.javaApi

/** Duplicates TestMasterProxy, to include a valid test logger in classpath. */
@javaApi
object TestJMasterProxyTest
{
  def main(args: Array[String]) = {
    TestJMasterProxy.main(args)
    println("Terminating")
  }
}
