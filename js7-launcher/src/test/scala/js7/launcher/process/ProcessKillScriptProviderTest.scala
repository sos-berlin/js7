package js7.launcher.process

import java.nio.file.Files
import java.nio.file.Files.*
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryRecursively, touchFile}
import js7.base.io.process.Processes.ShellFileExtension
import js7.base.system.OperatingSystem.isUnix
import js7.base.test.Test
import org.scalatest.BeforeAndAfterAll

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptProviderTest extends Test with BeforeAndAfterAll {

  private lazy val tmp = createTempDirectory("test-")
  private val expectedFile = tmp / s"kill_task$ShellFileExtension"

  override def afterAll() = {
    deleteDirectoryRecursively(tmp)
    super.afterAll()
  }

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
    touchFile(expectedFile)
    val provider = new ProcessKillScriptProvider
    provider.provideTo(tmp)
    assert(exists(expectedFile))
    provider.close()
    assert(!exists(expectedFile))
  }
}
