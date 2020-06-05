package js7.master.client.main

import java.nio.file.{Files, Path}
import js7.base.auth.SessionToken
import js7.base.convert.AsJava.StringAsPath
import js7.base.generic.SecretString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.log.Log4j
import js7.common.scalautil.Logger
import js7.master.client.AkkaHttpMasterTextApi
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
object MasterClientMain {

  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit =
    try {
      val rc = run(args.toVector, println)
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
    val (masterUri, configDir, dataDir, operations) = parseArgs(args)
    val sessionToken = SessionToken(SecretString(Files.readAllLines(dataDir resolve "state/session-token").asScala mkString ""))
    autoClosing(new AkkaHttpMasterTextApi(masterUri, None, print, configDir)) { textApi =>
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

  private def parseArgs(args: Seq[String]): (Uri, Option[Path], Path, Vector[Operation]) =
    CommandLineArguments.parse(args) { arguments =>
      val masterUri = Uri(arguments.keylessValue(0))
      val configDirectory = arguments.optionAs[Path]("-config-directory=")
      val dataDirectory = arguments.as[Path]("-data-directory=")
      val operations = arguments.keylessValues.tail map {
        case uri if uri.startsWith("?") || uri.startsWith("/") => Get(uri)
        case "-" => StdinCommand
        case command => StringCommand(command)
      }
      if ((operations count { _ == StdinCommand }) > 1) throw new IllegalArgumentException("Stdin ('-') can only be read once")
      (masterUri, configDirectory, dataDirectory, operations)
    }

  private sealed trait Operation
  private case class StringCommand(command: String) extends Operation
  private case object StdinCommand extends Operation
  private case class Get(uri: String) extends Operation
}
