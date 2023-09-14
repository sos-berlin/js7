package js7.controller.client.main

import java.nio.file.{Files, Path}
import js7.base.auth.SessionToken
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.io.process.ReturnCode
import js7.base.log.Logger
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.system.startup.JavaMain
import js7.controller.client.AkkaHttpControllerTextApi
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
object ControllerClientMain {

  private val logger = Logger[this.type]

  def main(args: Array[String]): Unit =
    try {
      val returnCode = run(args.toVector, println)
      JavaMain.exitIfNonZero(returnCode)
    }
    catch { case NonFatal(t) =>
      println(s"ERROR: $t")
      logger.error(t.toString, t)
      JavaMain.exit1()
    }

  def run(args: Seq[String], print: String => Unit): ReturnCode = {
    val (controllerUri, configDir, dataDir, operations) = parseArgs(args)
    val sessionToken = SessionToken(SecretString(Files.readAllLines(dataDir resolve "work/session-token").asScala mkString ""))
    autoClosing(new AkkaHttpControllerTextApi(controllerUri, None, print, configDir)) { textApi =>
      textApi.setSessionToken(sessionToken)
      if (operations.isEmpty)
        ReturnCode(if (textApi.checkIsResponding()) 0 else 1)
      else {
        operations foreach {
          case StringCommand(command) => textApi.executeCommand(command)
          case StdinCommand => textApi.executeCommand(scala.io.Source.stdin.mkString)
          case Get(uri) => textApi.getApi(uri)
        }
        ReturnCode(0)
      }
    }
  }

  private def parseArgs(args: Seq[String]): (Uri, Option[Path], Path, Vector[Operation]) =
    CommandLineArguments.parse(args) { arguments =>
      val controllerUri = Uri(arguments.keylessValue(0))
      val configDirectory = arguments.optionAs[Path]("--config-directory=")
      val dataDirectory = arguments.as[Path]("--data-directory=")
      val operations = arguments.keylessValues.tail map {
        case uri if uri.startsWith("?") || uri.startsWith("/") => Get(uri)
        case "-" => StdinCommand
        case command => StringCommand(command)
      }
      if (operations.count(_ == StdinCommand) > 1) throw new IllegalArgumentException("Stdin ('-') can only be read once")
      (controllerUri, configDirectory, dataDirectory, operations)
    }

  private sealed trait Operation
  private case class StringCommand(command: String) extends Operation
  private case object StdinCommand extends Operation
  private case class Get(uri: String) extends Operation
}
