package com.sos.jobscheduler.master

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder
import com.sos.jobscheduler.master.tests.TestEnvironment
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterMainIT extends FreeSpec {

  private lazy val httpPort = FreeTcpPortFinder.findRandomFreeTcpPort()

  "Simplistic test of start" in {
    autoClosing(new TestEnvironment(Nil, temporaryDirectory / "MasterMainIT")) { env ⇒
      val main = new MasterMain(List(
        "-data-directory=" + env.masterDir,
        "-http-port=" + httpPort))
      main.start() await 60.s
      main.close()
    }
  }
}
