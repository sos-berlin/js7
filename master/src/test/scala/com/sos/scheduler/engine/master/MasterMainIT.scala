package com.sos.scheduler.engine.master

import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.{FreeTcpPortFinder, JavaResource}
import com.sos.scheduler.engine.master.MasterMainIT._
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterMainIT extends FreeSpec {

  private lazy val httpPort = FreeTcpPortFinder.findRandomFreeTcpPort()

  "Simplistic test of start" in {
    val main = new MasterMain(List(
      "-data-directory=" + Paths.get(DataResource.uri),
      "-http-port=" + httpPort))
    main.start() await 60.s
    sleep(2.s)  // TODO Wait for successful startup
    main.close()
  }
}

object MasterMainIT {
  private val DataResource = JavaResource("com/sos/scheduler/engine/master/installation/data")
}
