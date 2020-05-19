package com.sos.jobscheduler.tests.master.proxy

import com.sos.jobscheduler.base.annotation.javaApi

/** Duplicates TestMasterProxy, to include a valid test logger in classpath. */
@javaApi
object TestJMasterProxyTest
{
  def main(args: Array[String]) = {
    TestJMasterProxy.main(args)
    println("Terminating")
  }
}
