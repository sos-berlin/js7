package com.sos.jobscheduler.agent.client.main

import com.sos.jobscheduler.agent.client.TextAgentClient
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.agent.AgentAddress
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
    catch { case NonFatal(t) ⇒
      println(s"ERROR: $t")
      logger.error(t.toString, t)
      Log4j.shutdown()
      sys.runtime.exit(1)
    }

  def run(args: Seq[String], print: String ⇒ Unit): Int = {
    val (agentUri, operations) = parseArgs(args)
    autoClosing(new TextAgentClient(agentUri, print)) { client ⇒
      if (operations.isEmpty)
        if (client.checkIsResponding()) 0 else 1
      else {
        operations foreach {
          case StringCommand(command) ⇒ client.executeCommand(command)
          case StdinCommand ⇒ client.executeCommand(scala.io.Source.stdin.mkString)
          case Get(uri) ⇒ client.get(uri)
        }
        0
      }
    }
  }

  private def parseArgs(args: Seq[String]) =
    CommandLineArguments.parse(args) { arguments ⇒
      val agentUri = AgentAddress.normalized(arguments.keylessValue(0))
      val operations = arguments.keylessValues.tail map {
        case url if url startsWith "/" ⇒ Get(url)
        case "-" ⇒ StdinCommand
        case command ⇒ StringCommand(command)
      }
      if((operations count { _ == StdinCommand }) > 1) throw new IllegalArgumentException("Stdin ('-') can only be read once")
      (agentUri, operations)
    }

  private sealed trait Operation
  private case class StringCommand(command: String) extends Operation
  private case object StdinCommand extends Operation
  private case class Get(uri: String) extends Operation
}
