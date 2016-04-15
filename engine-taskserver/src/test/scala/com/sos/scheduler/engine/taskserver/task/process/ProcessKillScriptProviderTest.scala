package com.sos.scheduler.engine.taskserver.task.process

import com.google.common.io.Files.touch
import com.sos.scheduler.engine.common.process.Processes
import com.sos.scheduler.engine.common.process.Processes.ShellFileExtension
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.isUnix
import java.nio.file.Files
import java.nio.file.Files._
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ProcessKillScriptProviderTest extends FreeSpec {

  private lazy val tmp = temporaryDirectory
  private val port = 99999
  private val expectedFile = tmp / s"jobscheduler_agent_${port}_kill_task$ShellFileExtension"

  "Do nothing" in {
    val provider = new ProcessKillScriptProvider(httpPort = port)
    provider.close()
  }

  "Provide script and delete it later" in {
    val provider = new ProcessKillScriptProvider(httpPort = port)
    deleteIfExists(expectedFile)
    val killScript = provider.provideTo(tmp)
    assert(killScript.file == expectedFile)
    assert(size(expectedFile) > 0)
    if (isUnix) {
      assert(Files.readAttributes(expectedFile, classOf[PosixFileAttributes]).permissions contains OWNER_EXECUTE)
    }
    provider.close()
    assert(!exists(expectedFile))
  }

  "Existing file is overwritten" in {
    touch(expectedFile)
    val provider = new ProcessKillScriptProvider(httpPort = port)
    provider.provideTo(tmp)
    assert(exists(expectedFile))
    provider.close()
    assert(!exists(expectedFile))
  }
}
