package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import java.net.{InetAddress, ServerSocket}
import org.scalatest.FreeSpec
import com.sos.scheduler.engine.common.time.ScalaTime._

/**
 * @author Joacim Zschimmer
 */
final class SimpleTaskServerTest extends FreeSpec {

  "SimpleTaskServer terminates when connection has been closed" in {
    val port = FreeTcpPortFinder.findRandomFreeTcpPort()
    val interface = "127.0.0.1"
    autoClosing(new ServerSocket(port, 1, InetAddress.getByName(interface))) { listener ⇒
      autoClosing(new SimpleTaskServer(TaskStartArguments.forTest(tcpPort = port))) { server ⇒
        server.start()
        listener.setSoTimeout(10*1000)
        listener.accept().close()   // The immediate close lets the task process abort, but we don't care.
        awaitResult(server.terminated, 1.s)
      }
    }
  }
}
