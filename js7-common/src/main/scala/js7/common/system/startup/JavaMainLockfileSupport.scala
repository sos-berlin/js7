package js7.common.system.startup

import cats.effect.IO
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.*
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.StandardOpenOption.{CREATE, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.io.file.FileUtils.tryDeleteDirectoryContentRecursively
import js7.base.io.process.ProcessPidRetriever
import js7.base.system.startup.StartUp.printlnWithClock
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object JavaMainLockfileSupport:

  // Do not call any function that may use a Logger before we own the lock file !!!
  // Because this could trigger an unexpected log file rotation.

  def runMain(name: String, args: Seq[String], useLockFile: Boolean = false)
    (body: CommandLineArguments => IO[ProgramTermination])
  : IO[ProgramTermination] =
    if useLockFile then
      lockAndRunMain(name, args)(body)
    else
      val arguments = CommandLineArguments(args)
      JavaMain.runMain(name):
        body(arguments)

  // Cleans also work directory
  /** Exit if lockFile is already locked. */
  private def lockAndRunMain(name: String, args: Seq[String])
    (body: CommandLineArguments => IO[ProgramTermination])
  : IO[ProgramTermination] =
    IO.defer:
      val arguments = CommandLineArguments(args)
      val data = Paths.get(arguments.as[String]("--data-directory="))
      val state = data.resolve("state")
      if !exists(state) then createDirectory(state)
      // The lockFile secures the state directory against double use.
      val lockFile = state.resolve("lock")
      lock(lockFile):
        JavaMain.runMain(name):
          cleanWorkDirectory(data.resolve("work"))
          body(arguments)

  private def cleanWorkDirectory(workDirectory: Path): Unit =
    if exists(workDirectory) then
      tryDeleteDirectoryContentRecursively(workDirectory)
    else
      createDirectory(workDirectory)

  // Also write PID to lockFile (Java >= 9)
  private def lock(lockFile: Path)(body: IO[ProgramTermination]): IO[ProgramTermination] =
    IO.defer:
      val lockFileChannel = FileChannel.open(lockFile, CREATE, WRITE)
      Try(lockFileChannel.tryLock()) match
        case Failure(throwable) =>
          IO:
            printlnWithClock(s"tryLock: $throwable")
            ProgramTermination.Failure

        case Success(null) =>
          IO:
            lockFileChannel.close()
            printlnWithClock("Duplicate start of JS7")
            ProgramTermination.Failure

        case Success(_) =>
          try
            for pid <- ProcessPidRetriever.maybeOwnPid do
              lockFileChannel.write(ByteBuffer.wrap(pid.string.getBytes(UTF_8)))
            body
          finally
            lockFileChannel.close()
          // Do not delete the lockFile, to avoid race condition with concurrent start.
