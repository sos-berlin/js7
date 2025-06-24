package js7.tests

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path, Paths}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceApp
import js7.data.controller.ControllerState
import js7.data.event.JournalHeader
import js7.journal.recover.StateRecoverer
import js7.tests.testenv.DirectoryProvider

object TestRecovery extends ServiceApp:

  private lazy val logger = Logger[this.type]

  def run(args: List[String]): IO[ExitCode] =
    runSimpleService(args, Conf.fromCommandLine(_))(program)

  private def program(conf: Conf): IO[Unit] =
    given IORuntime = runtime
    IO:
      val journalHeader = JournalHeader.readJournalHeader(conf.journalFile)
      val journalFilename = s"controller--${journalHeader.eventId}.journal"
      val controllerState = StateRecoverer.recoverFile[ControllerState](conf.journalFile)

      controllerState.emitLineStream(logger.info(_))
      val directoryProvider = DirectoryProvider(
        testName = Some("TestRecovery"),
        controllerConfig = config"""
          js7.controller.id = "${controllerState.controllerId.string}"
          """,
        agentPaths = Nil/*controllerState.keyToItem(AgentRef).keys.toSeq*/)
      createDirectory(directoryProvider.controllerEnv.stateDir)
      Files.copy(
        conf.journalFile,
        directoryProvider.controllerEnv.stateDir / journalFilename)
      directoryProvider.runController(dontWaitUntilReady = true): controller =>
        sleep(1.s)


  private final case class Conf(journalFile: Path) extends BasicConfiguration:
    val name = "Test"

    def config = ConfigFactory.empty()

  private object Conf:
    def fromCommandLine(args: CommandLineArguments): Conf =
      val conf = Conf(journalFile = Paths.get(args.keylessValue(0)))
      args.requireNoMoreArguments()
      conf
