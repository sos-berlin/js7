package js7.common.system.startup

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.*
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.StandardOpenOption.{CREATE, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.io.file.FileUtils.tryDeleteDirectoryContentRecursively
import js7.base.io.process.ProcessPidRetriever
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.StartUp.printlnWithClock
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object JavaMainLockfileSupport
{
  // Do not call any function that may use a Logger before we own the lock file !!!
  // Because this could trigger an unexpected log file rotation.

  def runMain[R](args: Array[String], useLockFile: Boolean = false)
    (body: CommandLineArguments => R)
  : R =
    if (useLockFile) {
      lockAndRunMain(args)(body)
    } else {
      val arguments = CommandLineArguments(args.toIndexedSeq)
      JavaMain.runMain {
        body(arguments)
      }
    }

  // Cleans also work directory
  /** Exit if lockFile is already locked. */
  def lockAndRunMain[R](args: Array[String])(body: CommandLineArguments => R): R = {
    val arguments = CommandLineArguments(args.toIndexedSeq)
    val data = Paths.get(arguments.as[String]("--data-directory="))
    val state = data.resolve("state")
    if (!exists(state)) createDirectory(state)
    // The lockFile secures the state directory against double use.
    val lockFile = state.resolve("lock")
    lock(lockFile) {
      JavaMain.runMain {
        cleanWorkDirectory(data.resolve("work"))
        body(arguments)
      }
    }
  }

  private def cleanWorkDirectory(workDirectory: Path): Unit =
    if (exists(workDirectory)) {
      tryDeleteDirectoryContentRecursively(workDirectory)
    } else {
      createDirectory(workDirectory)
    }

  // Also write PID to lockFile (Java >= 9)
  private def lock[R](lockFile: Path)(body: => R): R = {
    val lockFileChannel = FileChannel.open(lockFile, CREATE, WRITE)
    Try(lockFileChannel.tryLock()) match {
      case Failure(throwable) =>
        printlnWithClock(s"tryLock: $throwable")
        JavaMain.exit(1)

      case Success(null) =>
        lockFileChannel.close()
        printlnWithClock("Duplicate start of JS7")
        JavaMain.exit(1)

      case Success(_) =>
        try {
          for (pid <- ProcessPidRetriever.maybeOwnPid) {
            lockFileChannel.write(ByteBuffer.wrap(pid.string.getBytes(UTF_8)))
          }
          body
        } finally lockFileChannel.close()
        // Do not delete the lockFile, to avoid race condition with concurrent start.
    }
  }
}
