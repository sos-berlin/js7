package js7.agent.client.main

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.{Files, Path}
import js7.agent.client.PekkoHttpAgentTextApi
import js7.base.auth.SessionToken
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.SimpleServiceProgram
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*

/**
 * @author Joacim Zschimmer
 */
object AgentClientMain extends SimpleServiceProgram[AgentClientMain.Conf]:

  override protected val productName = "AgentClient"

  protected def program(conf: Conf): IO[ExitCode] =
    program(conf, println)(using runtime)

  def program(conf: Conf, print: String => Unit)(using ioRuntime: IORuntime): IO[ExitCode] =
    given ExecutionContext = ioRuntime.compute
    IO.interruptible:
      import conf.{agentUri, dataDirectory, maybeConfigDirectory, operations}
      val sessionToken = SessionToken(SecretString:
        Files.readAllLines(dataDirectory.resolve("work/session-token")).asScala.mkString)
      autoClosing(PekkoHttpAgentTextApi(agentUri, None, print, maybeConfigDirectory)): textApi =>
        textApi.setSessionToken(sessionToken)
        if operations.isEmpty then
          if textApi.checkIsResponding() then ExitCode.Success else ExitCode.Error
        else
          operations foreach:
            case StringCommand(command) => textApi.executeCommand(command)
            case StdinCommand => textApi.executeCommand(scala.io.Source.stdin.mkString)
            case Get(uri) => textApi.getApi(uri)
          ExitCode.Success

  sealed trait Operation
  final case class StringCommand(command: String) extends Operation
  case object StdinCommand extends Operation
  final case class Get(uri: String) extends Operation

  final case class Conf(
    agentUri: Uri,
    override val maybeConfigDirectory: Option[Path],
    dataDirectory: Path,
    operations: Seq[Operation])
  extends BasicConfiguration:
    val config: Config = ConfigFactory.empty

    val name = "AgentClient"

  object Conf extends BasicConfiguration.Companion[Conf]:
    def fromCommandLine(args: CommandLineArguments): Conf =
      val agentUri = Uri(args.keylessValue(0))
      val configDirectory = args.optionAs[Path]("--config-directory=")
      val dataDirectory = args.as[Path]("--data-directory=")
      val operations = args.keylessValues.tail.map:
        case url if url.startsWith("?") || url.startsWith("/") => Get(url)
        case "-" => StdinCommand
        case command => StringCommand(command)
      if operations.count(_ == StdinCommand) > 1 then
        throw new IllegalArgumentException("Stdin ('-') can only be read once")
      Conf(agentUri, configDirectory, dataDirectory, operations)
