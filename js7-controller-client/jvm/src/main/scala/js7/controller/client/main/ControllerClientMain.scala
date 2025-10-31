package js7.controller.client.main

import cats.effect.{ExitCode, IO}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.auth.SessionToken
import js7.base.config.Js7Config
import js7.base.configutils.Configs.parseConfigIfExists
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.SimpleServiceProgram
import js7.controller.client.PekkoHttpControllerTextApi
import scala.jdk.CollectionConverters.*

/**
 * @author Joacim Zschimmer
 */
object ControllerClientMain extends SimpleServiceProgram[ControllerClientMain.Conf]:

  protected def program(conf: Conf): IO[ExitCode] =
    program(conf, println)

  def program(conf: Conf, print: String => Unit): IO[ExitCode] =
    IO.defer:
      import conf.{operations, readWorkFile}
      val controllerUri = Uri.mayThrow(readWorkFile("http-uri"))
      val sessionToken = SessionToken(SecretString(readWorkFile("session-token")))
      PekkoHttpControllerTextApi.resource(controllerUri, None, print, conf).use: textApi =>
        textApi.setSessionToken(sessionToken)
        if operations.isEmpty then
          textApi.checkIsResponding.map: is =>
            if is then ExitCode.Success else ExitCode.Error
        else
          operations.foldMap:
            case StringCommand(command) => textApi.executeCommand(command)
            case StdinCommand => textApi.executeCommand(scala.io.Source.stdin.mkString)
            case Get(uri) => textApi.getApi(uri)
          .as(ExitCode(0))

  sealed trait Operation
  final case class StringCommand(command: String) extends Operation
  case object StdinCommand extends Operation
  final case class Get(uri: String) extends Operation


  final case class Conf(
    configDirectory: Path,
    dataDirectory: Path,
    operations: Seq[Operation])
  extends BasicConfiguration:
    override def maybeConfigDirectory = Some(configDirectory)

    val config: Config =
      // Like ControllerConfiguration.configDirectoryToConfig
      ConfigFactory.parseMap:
        Map(
          "js7.config-directory" -> configDirectory.toString,
          "js7.data-directory" -> dataDirectory.toString
        ).asJava
      .withFallback(ConfigFactory.systemProperties)
      .withFallback(parseConfigIfExists(configDirectory / "private" / "private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "controller.conf", secret = false))
      .withFallback(Js7Config.defaultConfig)

    val name = "ControllerClient"

    def readWorkFile(file: String): String =
      dataDirectory.resolve(Path.of("work", file)).contentString.trim

  object Conf extends BasicConfiguration.Companion[Conf]:
    def fromCommandLine(args: CommandLineArguments): Conf =
      val configDirectory = args.as[Path]("--config-directory=")
      val dataDirectory = args.as[Path]("--data-directory=")
      val operations = args.keylessValues.map:
        case url if url.startsWith("?") || url.startsWith("/") => Get(url)
        case "-" => StdinCommand
        case command => StringCommand(command)
      if operations.count(_ == StdinCommand) > 1 then
        throw new IllegalArgumentException("Stdin ('-') can only be read once")
      Conf(configDirectory, dataDirectory, operations)
