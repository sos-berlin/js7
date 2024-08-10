package js7.controller.client.main

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.{Files, Path}
import js7.base.auth.SessionToken
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceApp
import js7.controller.client.PekkoHttpControllerTextApi
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*

/**
 * @author Joacim Zschimmer
 */
object ControllerClientMain extends ServiceApp:

  private given IORuntime = runtime
  private given ExecutionContext = runtime.compute

  def run(args: List[String]): IO[ExitCode] =
    runProgramAsService(args, Conf.fromCommandLine)(program)

  protected def program(conf: Conf): IO[ExitCode] =
    program(conf, println)

  def program(conf: Conf, print: String => Unit): IO[ExitCode] =
    IO.interruptible:
      import conf.{controllerUri, dataDirectory, maybeConfigDirectory, operations}
      val sessionToken = SessionToken(SecretString:
        Files.readAllLines(dataDirectory resolve "work/session-token")
          .asScala.mkString)
      autoClosing(PekkoHttpControllerTextApi(controllerUri, None, print, maybeConfigDirectory)):
        textApi =>
          textApi.setSessionToken(sessionToken)
          if operations.isEmpty then
            ExitCode(if textApi.checkIsResponding() then 0 else 1)
          else
            operations.foreach:
              case StringCommand(command) => textApi.executeCommand(command)
              case StdinCommand => textApi.executeCommand(scala.io.Source.stdin.mkString)
              case Get(uri) => textApi.getApi(uri)
            ExitCode(0)

  sealed trait Operation
  final case class StringCommand(command: String) extends Operation
  case object StdinCommand extends Operation
  final case class Get(uri: String) extends Operation

  final case class Conf(
    controllerUri: Uri,
    override val maybeConfigDirectory: Option[Path],
    dataDirectory: Path,
    operations: Seq[Operation])
  extends BasicConfiguration:
    val config: Config = ConfigFactory.empty

  object Conf:
    def args(args: String*): Conf =
      CommandLineArguments.parse(args)(fromCommandLine)

    def fromCommandLine(args: CommandLineArguments): Conf =
      val controllerUri = Uri(args.keylessValue(0))
      val configDirectory = args.optionAs[Path]("--config-directory=")
      val dataDirectory = args.as[Path]("--data-directory=")
      val operations = args.keylessValues.tail.map:
        case url if url.startsWith("?") || url.startsWith("/") => Get(url)
        case "-" => StdinCommand
        case command => StringCommand(command)
      if operations.count(_ == StdinCommand) > 1 then
        throw new IllegalArgumentException("Stdin ('-') can only be read once")
      Conf(controllerUri, configDirectory, dataDirectory, operations)
