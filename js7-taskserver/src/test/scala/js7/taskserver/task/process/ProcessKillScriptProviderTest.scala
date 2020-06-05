package js7.taskserver.task.process

import com.google.common.io.MoreFiles.touch
import js7.common.process.Processes.ShellFileExtension
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.system.OperatingSystem.isUnix
import java.nio.file.Files
import java.nio.file.Files._
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptProviderTest extends AnyFreeSpec with BeforeAndAfterAll {

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
    touch(expectedFile)
    val provider = new ProcessKillScriptProvider
    provider.provideTo(tmp)
    assert(exists(expectedFile))
    provider.close()
    assert(!exists(expectedFile))
  }
}
