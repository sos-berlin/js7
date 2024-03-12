package js7.agent.client.main

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import java.nio.file.{Files, Path}
import js7.agent.client.PekkoHttpAgentTextApi
import js7.base.auth.SessionToken
import js7.base.catsutils.OurApp
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMain
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*

/**
 * @author Joacim Zschimmer
 */
object AgentClientMain extends OurApp:

  private given IORuntime = runtime
  private given ExecutionContext = runtime.compute

  def run(args: List[String]) =
    JavaMain.run("AgentClient")(IO:
      run(args.toIndexedSeq, println))

  def run(args: Seq[String], print: String => Unit)(using IORuntime /*for testing*/): ExitCode =
    val (agentUri, configDirectory, dataDir, operations) = parseArgs(args)
    val sessionToken = SessionToken(SecretString(Files.readAllLines(dataDir resolve "work/session-token").asScala mkString ""))
    autoClosing(new PekkoHttpAgentTextApi(agentUri, None, print, configDirectory)) { textApi =>
      textApi.setSessionToken(sessionToken)
      if operations.isEmpty then
        ExitCode(if textApi.checkIsResponding() then 0 else 1)
      else
        operations foreach :
          case StringCommand(command) => textApi.executeCommand(command)
          case StdinCommand => textApi.executeCommand(scala.io.Source.stdin.mkString)
          case Get(uri) => textApi.getApi(uri)
        ExitCode(0)
    }

  private def parseArgs(args: Seq[String]) =
    CommandLineArguments.parse(args) { arguments =>
      val agentUri = Uri(arguments.keylessValue(0))
      val configDirectory = arguments.optionAs[Path]("--config-directory=")
      val dataDirectory = arguments.as[Path]("--data-directory=")
      val operations = arguments.keylessValues.tail map:
        case url if url.startsWith("?") || url.startsWith("/") => Get(url)
        case "-" => StdinCommand
        case command => StringCommand(command)
      if operations.count(_ == StdinCommand) > 1 then throw new IllegalArgumentException("Stdin ('-') can only be read once")
      (agentUri, configDirectory, dataDirectory, operations)
    }

  private sealed trait Operation
  private case class StringCommand(command: String) extends Operation
  private case object StdinCommand extends Operation
  private case class Get(uri: String) extends Operation
