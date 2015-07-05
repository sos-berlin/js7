package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import java.net.ServerSocket
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SeparateProcessTaskServerTest extends FreeSpec {

  "SeperateTaskServerProcess" in {
    val tcpPort = findRandomFreeTcpPort()
    autoClosing(new ServerSocket(tcpPort, 1)) { listener ⇒
      val taskArguments = TaskStartArguments.forTest(tcpPort = tcpPort)
      autoClosing(new SeparateProcessTaskServer(taskArguments, javaOptions = Nil, javaClasspath = "")) { process ⇒
        process.start()
        listener.setSoTimeout(10*1000)
        listener.accept().close()  // The immediate close lets the task process abort, but we don't care.
      }
    }
  }
}
