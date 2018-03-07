package com.sos.jobscheduler.tests

import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.master.client.main.MasterClientMain
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class MasterClientMainTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  protected val agentPaths = Nil

  "main with Master URI only checks wether Master is responding (it is)" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      MasterClientMain.run(List(master.localUri.toString), output.+=)
    }
    assert(output == List("JobScheduler Master is responding"))
  }

  "Multiple api calls" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      MasterClientMain.run(List(master.localUri.toString, "?", "/order"), output.+=)
    }
    assert(output(0) contains "version:")
    assert(output(1) == "---")
    assert(output(2) contains "orderCount: 0")
  }

  "main with Master URI only checks wether Master is responding (it is not)" in {
    val port = findRandomFreeTcpPort()
    val output = mutable.Buffer[String]()
    assertResult(1) {
      MasterClientMain.run(List(s"http://127.0.0.1:$port"), output += _)
    }
    assert(output.head contains "JobScheduler Master is not responding: ")
    assert(output.head contains "Connection refused")
  }

  "Terminate" in {
    val output = mutable.Buffer[String]()
    val commandYaml = """{ TYPE: Terminate }"""
    MasterClientMain.run(List(master.localUri.toString, commandYaml), output.+=)
    assert(output == List("TYPE: Accepted"))
    master.terminated await 99.s
  }
}

