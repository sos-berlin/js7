package js7.tests

import cats.effect.{ExitCode, IO, ResourceIO}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.{Path, Paths}
import js7.base.log.Logger
import js7.base.service.{MainService, Service}
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceApp
import js7.data.controller.ControllerState
import js7.journal.recover.StateRecoverer

object TestRecovery extends ServiceApp:

  private lazy val logger = Logger[this.type]

  def run(args: List[String]): IO[ExitCode] =
    runService(args, Conf.fromCommandLine(_)):
      resource(_)

  private def resource(conf: Conf): ResourceIO[MainService] =
    Service.resource(IO(Service.simple(program(conf))))

  private def program(conf: Conf): IO[Unit] =
    IO:
      val controllerState = StateRecoverer.recoverFile[ControllerState](conf.journalFile)
      controllerState.emitLineStream(logger.info(_))


  private final case class Conf(journalFile: Path) extends BasicConfiguration:
    def config = ConfigFactory.empty()

  private object Conf:
    def fromCommandLine(args: CommandLineArguments): Conf =
      val conf = Conf(journalFile = Paths.get(args.keylessValue(0)))
      args.requireNoMoreArguments()
      conf
