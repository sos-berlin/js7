package js7.common.system.startup

import cats.effect.{IO, Resource}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.*
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.StandardOpenOption.{CREATE, WRITE}
import java.nio.file.{Path, Paths}
import js7.base.io.file.FileUtils.tryDeleteDirectoryContentRecursively
import js7.base.system.startup.StartUp.printlnWithClock
import js7.base.utils.ProgramTermination
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object JavaMainLockfileSupport:

  // Do not call any function that may use a Logger before we own the lock file !!!
  // Because this could trigger an unexpected log file rotation.

  def runMain(args: Seq[String], useLockFile: Boolean = false)
    (body: CommandLineArguments => IO[ProgramTermination])
  : IO[ProgramTermination] =
    if useLockFile then
      lockAndRunMain(args):
        body
    else
      JavaMain.runMain(args):
        body

  /** Exit if lockFile is already locked.
    * Cleans also the `workDirectory`.
    */
  private def lockAndRunMain(args: Seq[String])
    (body: CommandLineArguments => IO[ProgramTermination])
  : IO[ProgramTermination] =
    IO.defer:
      val arguments = CommandLineArguments(args)
      val data = Paths.get(arguments.as[String]("--data-directory="))
      val state = data.resolve("state")
      if !exists(state) then createDirectory(state)
      // The lockFile secures the state directory against double use.
      val lockFile = BasicConfiguration.dataToLockFile(data)
      lock(lockFile, ProgramTermination.Failure):
        JavaMain.runMain:
          cleanWorkDirectory(data.resolve("work"))
          body(arguments)

  private def cleanWorkDirectory(workDirectory: Path): Unit =
    if exists(workDirectory) then
      tryDeleteDirectoryContentRecursively(workDirectory)
    else
      createDirectory(workDirectory)

  // Also writes PID to lockFile
  // Do not delete the lockFile
  // - to avoid race condition with concurrent start,
  // - to allow the calling script to compare its content after the Engine terminated.
  //   (Used by CrashPidFileKiller)
  def lock[A](lockFile: Path, lockedValue: A)(body: IO[A]): IO[A] =
    Resource
      .fromAutoCloseable(IO:
        FileChannel.open(lockFile, CREATE, WRITE))
      .use: lockFileChannel =>
        Try(lockFileChannel.tryLock()) match
          case Failure(throwable) =>
            IO.blocking:
              printlnWithClock(s"tryLock: $throwable")
              lockedValue

          case Success(null) =>
            IO.blocking:
              printlnWithClock("Duplicate start of JS7")
              lockedValue

          case Success(_) =>
            IO.blocking:
              lockFileChannel.write:
                ByteBuffer.wrap:
                  ProcessHandle.current.pid.toString.getBytes(UTF_8)
            *> body
