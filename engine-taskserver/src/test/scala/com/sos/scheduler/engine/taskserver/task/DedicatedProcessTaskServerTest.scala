package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class DedicatedProcessTaskServerTest extends FreeSpec {

  "DedicatedTaskServerProcess" in {
    val taskArguments = TaskStartArguments(controllerAddress = "127.0.0.1:9999")
    autoClosing(new DedicatedProcessTaskServer(taskArguments, javaOptions = "", javaClasspath = "")) { process ⇒
      process.start()
      process.close()
    }
  }
}

object DedicatedProcessTaskServerTest {
}
