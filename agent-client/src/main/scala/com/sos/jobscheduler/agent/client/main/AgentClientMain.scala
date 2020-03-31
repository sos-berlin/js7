package com.sos.jobscheduler.agent.client.main

import com.sos.jobscheduler.agent.client.AkkaHttpAgentTextApi
import com.sos.jobscheduler.base.auth.SessionToken
import com.sos.jobscheduler.base.convert.AsJava.StringAsPath
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.Logger
import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._
import scala.io
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
object AgentClientMain {

  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit =
    try {
      val rc = run(args, println)
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
    val sessionToken = SessionToken(SecretString(Files.readAllLines(dataDir resolve "state/session-token").asScala mkString ""))
    autoClosing(new AkkaHttpAgentTextApi(agentUri, print, configDirectory)) { textApi =>
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
      val configDirectory = arguments.optionAs[Path]("-config-directory=")
      val dataDirectory = arguments.as[Path]("-data-directory=")
      val operations = arguments.keylessValues.tail map {
        case url if url.startsWith("?") || url.startsWith("/") => Get(url)
        case "-" => StdinCommand
        case command => StringCommand(command)
      }
      if ((operations count { _ == StdinCommand }) > 1) throw new IllegalArgumentException("Stdin ('-') can only be read once")
      (agentUri, configDirectory, dataDirectory, operations)
    }

  private sealed trait Operation
  private case class StringCommand(command: String) extends Operation
  private case object StdinCommand extends Operation
  private case class Get(uri: String) extends Operation
}
