package js7.launcher.crashpidfile

import cats.effect.{ExitCode, IO, Resource, ResourceIO}
import com.typesafe.config.{Config, ConfigFactory}
import fs2.{Pipe, Stream}
import java.io.{FileInputStream, InputStream}
import java.nio.BufferUnderflowException
import java.nio.file.Path
import js7.base.convert.AsJava.StringAsPath
import js7.base.io.ReaderStreams.inputStreamToByteStream
import js7.base.io.process.Pid
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.{JavaMainLockfileSupport, ServiceApp}
import js7.launcher.processkiller.CrashProcessKiller
import scala.concurrent.duration.FiniteDuration

object CrashPidFileKiller extends ServiceApp:

  private lazy val logger = Logger[this.type]

  def run(args: List[String]): IO[ExitCode] =
    runProgramAsService(args, Conf.fromCommandLine): conf =>
      conf.maybeDataDirectory match
        case None =>
          program(conf)

        case Some(data) =>
          // Use the same lock file as Subagent
          val lockFile = BasicConfiguration.dataToLockFile(data)
          JavaMainLockfileSupport.lock(lockFile, ExitCode.Error):
            program(conf)

  private def program(conf: Conf): IO[ExitCode] =
    inputStream(conf).use: in =>
      program(conf, in).as(ExitCode.Success)

  private def inputStream(conf: Conf): ResourceIO[InputStream] =
    conf.maybeDataDirectory match
      case None => Resource.eval(IO.pure(System.in))
      case Some(dir) =>
        Resource.fromAutoCloseable(IO(FileInputStream(CrashPidFile.dataDirToFile(dir).toFile)))

  def program(conf: Conf, in: InputStream): IO[Unit] =
    inputStreamToByteStream(in)
      .through(recordToPid)
      .through(logAndIgnoreProblem)
      .compile
      .toVector
      .flatMap: pids =>
        CrashProcessKiller
          .resource(
            dontExecute = conf.dontExecute,
            sigkillDelay = conf.sigkillDelay)
          .use:
            _.killWithDescendants(pids)

  private def recordToPid: Pipe[IO, Byte, Checked[Pid]] =
    _.chunkN(8).map: chunk =>
      try
        Right(Pid:
          chunk.toByteBuffer.order(CrashPidFile.byteOrder).getLong)
      catch case _: BufferUnderflowException =>
        Left(Problem("Input file is truncated"))

  private def logAndIgnoreProblem[A]: Pipe[IO, Checked[A], A] =
    _.flatMap:
      case Left(problem) =>
        logger.error(problem.toString)
        Stream.empty
      case Right(o) =>
        Stream.emit(o)


  final case class Conf(
    override val maybeDataDirectory: Option[Path] = None,
    dontExecute: Boolean = false,
    sigkillDelay: FiniteDuration = ZeroDuration)
  extends BasicConfiguration:
    val config: Config = ConfigFactory.empty

    def name = "CrashPidFileKiller"

  object Conf:
    val Default: Conf = Conf()
    val DataDirOption = "--data-directory="

    def fromCommandLine(args: CommandLineArguments): Conf =
      val conf = Conf(
        maybeDataDirectory = args.optionAs[Path](DataDirOption),
        dontExecute = args.boolean("-n"),
        sigkillDelay = args.as[FiniteDuration]("--sigkill-delay=", Default.sigkillDelay))
      args.requireNoMoreArguments()
      conf
