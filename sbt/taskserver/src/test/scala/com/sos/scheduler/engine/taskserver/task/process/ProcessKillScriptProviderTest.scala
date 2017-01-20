package com.sos.scheduler.engine.taskserver.task.process

import com.google.common.io.Files.touch
import com.sos.scheduler.engine.common.process.Processes.ShellFileExtension
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.isUnix
import java.nio.file.Files
import java.nio.file.Files._
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import org.scalatest.FreeSpec

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptProviderTest extends FreeSpec {

  private lazy val tmp = temporaryDirectory
  private val expectedFile = tmp / s"kill_task$ShellFileExtension"

  "Do nothing" in {
    val provider = new ProcessKillScriptProvider
    provider.close()
  }

  "Provide script and delete it later" in {
    val provider = new ProcessKillScriptProvider
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
    val provider = new ProcessKillScriptProvider
    provider.provideTo(tmp)
    assert(exists(expectedFile))
    provider.close()
    assert(!exists(expectedFile))
  }
}
