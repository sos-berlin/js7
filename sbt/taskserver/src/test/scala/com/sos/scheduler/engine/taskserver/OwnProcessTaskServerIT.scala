package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.taskserver.data.TaskServerArguments
import java.net.ServerSocket
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 */
final class OwnProcessTaskServerIT extends FreeSpec {

  "OwnProcessTaskServer" in {
    val tcpPort = findRandomFreeTcpPort()
    autoClosing(new ServerSocket(tcpPort, 1)) { listener ⇒
      val taskArguments = TaskServerArguments.forTest(tcpPort = tcpPort)
      autoClosing(new OwnProcessTaskServer(taskArguments, javaOptions = Nil, javaClasspath = "")) { process ⇒
        process.start()
        listener.setSoTimeout(10*1000)
        listener.accept().close()  // The immediate close lets the task process abort, but we don't care.
      }
    }
  }
}
