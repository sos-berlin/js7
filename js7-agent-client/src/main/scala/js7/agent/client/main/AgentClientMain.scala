package js7.agent.client.main

import cats.effect.kernel.Resource
import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import com.typesafe.config.ConfigFactory
import java.nio.file.{Files, Path}
import js7.agent.client.PekkoHttpAgentTextApi
import js7.base.auth.SessionToken
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.service.Service
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ProgramTermination
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.ServiceApp
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*

/**
 * @author Joacim Zschimmer
 */
object AgentClientMain extends ServiceApp:

  override protected def productName = "JS7 AgentClient"

  private given IORuntime = runtime
  private given ExecutionContext = runtime.compute

  def run(args: List[String]): IO[ExitCode] =
    runService(args, Conf.fromCommandLine):
      conf => Resource.eval(IO.pure(Service.simple(IO(run(conf, println)))))

  def run(
    conf: Conf,
    print: String => Unit)
    (using IORuntime /*for testing*/)
  : ProgramTermination =
    import conf.{agentUri, dataDirectory, maybeConfigDirectory, operations}
    val sessionToken = SessionToken(SecretString(Files.readAllLines(dataDirectory resolve "work/session-token").asScala mkString ""))
    autoClosing(new PekkoHttpAgentTextApi(agentUri, None, print, maybeConfigDirectory)): textApi =>
      textApi.setSessionToken(sessionToken)
      if operations.isEmpty then
        if textApi.checkIsResponding()
        then ProgramTermination.Success
        else ProgramTermination.Failure
      else
        operations foreach :
          case StringCommand(command) => textApi.executeCommand(command)
          case StdinCommand => textApi.executeCommand(scala.io.Source.stdin.mkString)
          case Get(uri) => textApi.getApi(uri)
        ProgramTermination.Success

  sealed trait Operation
  case class StringCommand(command: String) extends Operation
  case object StdinCommand extends Operation
  case class Get(uri: String) extends Operation

  final case class Conf(
    agentUri: Uri,
    override val maybeConfigDirectory: Option[Path],
    dataDirectory: Path,
    operations: Seq[Operation])
  extends BasicConfiguration:
    val config = ConfigFactory.empty

  object Conf:
    def fromCommandLine(args: CommandLineArguments): Conf =
      val agentUri = Uri(args.keylessValue(0))
      val configDirectory = args.optionAs[Path]("--config-directory=")
      val dataDirectory = args.as[Path]("--data-directory=")
      val operations = args.keylessValues.tail map :
        case url if url.startsWith("?") || url.startsWith("/") => Get(url)
        case "-" => StdinCommand
        case command => StringCommand(command)
      if operations.count(_ == StdinCommand) > 1 then
        throw new IllegalArgumentException("Stdin ('-') can only be read once")
      Conf(agentUri, configDirectory, dataDirectory, operations)
