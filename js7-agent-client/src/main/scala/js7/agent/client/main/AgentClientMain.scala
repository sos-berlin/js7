package js7.agent.client.main

import java.nio.file.{Files, Path}
import js7.agent.client.AkkaHttpAgentTextApi
import js7.base.auth.SessionToken
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.log.{Log4j, Logger}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
object AgentClientMain
{
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit =
    try {
      val rc = run(args.toIndexedSeq, println)
      Log4j.shutdown()
      sys.runtime.exit(rc)
    }
    catch { case NonFatal(t) =>
      println(s"ERROR: $t")
      logger.error(t.toString, t)
      Log4j.shutdown()
      sys.runtime.exit(1)
    }

  def run(args: Seq[String], print: String => Unit): Int = {
    val (agentUri, configDirectory, dataDir, operations) = parseArgs(args)
    val sessionToken = SessionToken(SecretString(Files.readAllLines(dataDir resolve "work/session-token").asScala mkString ""))
    autoClosing(new AkkaHttpAgentTextApi(agentUri, None, print, configDirectory)) { textApi =>
      textApi.setSessionToken(sessionToken)
      if (operations.isEmpty)
        if (textApi.checkIsResponding()) 0 else 1
      else {
        operations foreach {
          case StringCommand(command) => textApi.executeCommand(command)
          case StdinCommand => textApi.executeCommand(scala.io.Source.stdin.mkString)
          case Get(uri) => textApi.getApi(uri)
        }
        0
      }
    }
  }

  private def parseArgs(args: Seq[String]) =
    CommandLineArguments.parse(args) { arguments =>
      val agentUri = Uri(arguments.keylessValue(0))
      val configDirectory = arguments.optionAs[Path]("--config-directory=")
      val dataDirectory = arguments.as[Path]("--data-directory=")
      val operations = arguments.keylessValues.tail map {
        case url if url.startsWith("?") || url.startsWith("/") => Get(url)
        case "-" => StdinCommand
        case command => StringCommand(command)
      }
      if (operations.count(_ == StdinCommand) > 1) throw new IllegalArgumentException("Stdin ('-') can only be read once")
      (agentUri, configDirectory, dataDirectory, operations)
    }

  private sealed trait Operation
  private case class StringCommand(command: String) extends Operation
  private case object StdinCommand extends Operation
  private case class Get(uri: String) extends Operation
}
