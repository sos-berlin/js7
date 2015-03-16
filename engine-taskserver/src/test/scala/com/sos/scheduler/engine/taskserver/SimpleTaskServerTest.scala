package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import java.net.{InetAddress, ServerSocket}
import org.scalatest.FreeSpec
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class SimpleTaskServerTest extends FreeSpec {

  "SimpleTaskServer terminates when connection has been closed" in {
    val port = FreeTcpPortFinder.findRandomFreeTcpPort()
    val interface = "127.0.0.1"
    autoClosing(new ServerSocket(port, 1, InetAddress.getByName(interface))) { listener ⇒
      autoClosing(new SimpleTaskServer(TaskStartArguments(s"$interface:$port"))) { server ⇒
        server.start()
        listener.accept().close()
        Await.result(server.terminated, 1.seconds)
      }
    }
  }
}
