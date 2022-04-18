package js7.common.system.startup

import java.nio.channels.FileChannel
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.StandardOpenOption.{CREATE, WRITE}
import java.nio.file.{Path, Paths}
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.StartUp.printlnWithClock
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object JavaMainLockfileSupport
{
  // Do not call any function that may use a Logger !!!

  /** Exits program if lockfile is already locked. */
  def lockAndRunMain(args: Array[String])(body: CommandLineArguments => Unit): Unit =
    CommandLineArguments.parse(args.toIndexedSeq) { arguments =>
      val dataDirectory = Paths.get(arguments.as[String]("--data-directory="))
      val stateDirectory = dataDirectory.resolve("state")
      // lock file is not placed in workDirectory.
      // Instead we place the lock file in the stateDirectory.
      // So we can delete the whole workDirectory content at start.
      if (!exists(stateDirectory)) {
        createDirectory(stateDirectory)
      }
      lock(stateDirectory resolve "lock") {
        val workDirectory = dataDirectory.resolve("work")
        val pidFile = workDirectory.resolve("pid")
        if (exists(workDirectory)) {
          deleteIfExists(pidFile)
          tryDeleteDirectoryContentRecursively(workDirectory)
        } else {
          createDirectory(workDirectory)
        }
        JavaMain.runMain {
          body(arguments)
        }
      }
    }

  private def lock(lockFile: Path)(body: => Unit): Unit = {
    val lockFileChannel = FileChannel.open(lockFile, CREATE, WRITE)
    Try(lockFileChannel.tryLock()) match {
      case Success(null) =>
        lockFileChannel.close()
        printlnWithClock("Duplicate start of JS7")
        System.exit(1)

      case Failure(throwable) =>
        printlnWithClock(s"tryLock: $throwable")
        System.exit(1)

      case Success(_) =>
        try body
        finally try {
          lockFileChannel.close()
          lockFile.toFile.delete()
        } catch { case NonFatal(t) => println(t.toString) }
    }
  }
}
