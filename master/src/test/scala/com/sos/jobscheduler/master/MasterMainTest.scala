package com.sos.jobscheduler.master

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.tests.TestEnvironment
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterMainTest extends FreeSpec {

  private lazy val httpPort = FreeTcpPortFinder.findRandomFreeTcpPort()

  "Simplistic test of start" in {
    autoClosing(new TestEnvironment(Nil, temporaryDirectory / "MasterMainIT")) { env ⇒
      (for (runningMaster ← MasterMain.start(MasterConfiguration.fromCommandLine(List(
        "-data-directory=" + env.masterDir,
        "-config-directory=" + env.masterDir / "config",
        "-http-port=" + httpPort))))
      yield {
        runningMaster.executeCommand(MasterCommand.Terminate) await 99.s
        runningMaster.terminated await 99.s
        runningMaster.close()
      }) await 99.s
    }
  }
}
