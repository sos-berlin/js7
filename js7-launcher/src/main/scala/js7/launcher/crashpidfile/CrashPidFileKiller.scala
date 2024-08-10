package js7.launcher.crashpidfile

import cats.effect.{ExitCode, IO}
import com.typesafe.config.ConfigFactory
import fs2.{Pipe, Stream}
import java.io.InputStream
import java.nio.BufferUnderflowException
import js7.base.io.process.Pid
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceApp
import js7.launcher.process.CrashProcessKiller

object CrashPidFileKiller extends ServiceApp:

  private lazy val logger = Logger[this.type]

  def run(args: List[String]): IO[ExitCode] =
    runProgramAsService(args, Conf.fromCommandLine)(program)

  protected def program(conf: Conf): IO[ExitCode] =
    program(conf, System.in).as(ExitCode.Success)

  def program(conf: Conf, in: InputStream): IO[Unit] =
    IO.defer:
      fs2.io.readInputStream(IO.pure(in), chunkSize = 512, closeAfterUse = false)
        .through(recordToPid)
        .through(logAndIgnoreProblem)
        .compile
        .toVector
        .flatMap: pids =>
          CrashProcessKiller(dontExecute = conf.dontExecute)
            .sigkillWithDescendants(pids)

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

  final case class Conf(dontExecute: Boolean = false)
  extends BasicConfiguration:
    val config = ConfigFactory.empty


  object Conf:
    def fromCommandLine(args: CommandLineArguments): Conf =
      val conf = Conf(dontExecute = args.boolean("-n"))
      args.requireNoMoreArguments()
      conf
