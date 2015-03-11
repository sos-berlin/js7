package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.commands.{Command, StartRemoteTask, StartRemoteTaskResponse}
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutorTest._
import com.sos.scheduler.engine.data.agent.RemoteTaskId
import java.net.{InetAddress, InetSocketAddress}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CommandXmlExecutorTest extends FreeSpec {

  "CommandXmlExecutor" in {
    def executeCommand(command: Command) = command match {
      case StartRemoteTask(ASocketAddress, true, "", "") ⇒ Future { throw new Exception }
      case StartRemoteTask(BSocketAddress, true, "", "") ⇒ Future { StartRemoteTaskResponse(RemoteTaskId(123)) }
      case o ⇒ fail(o.toString)
    }
    def execute(command: String): Unit = {
      val executed = new CommandXmlExecutor(executeCommand).execute(IP, command)
      Await.result(executed, 1.seconds) match {
        case <spooler><answer>{elem: xml.Elem}</answer></spooler> ⇒ if (elem.label == "ERROR") throw new CommandException
      }
    }
    intercept[CommandException] { execute("INVALID XML") }
    intercept[CommandException] { execute("<WRONG/>") }
    intercept[CommandException] { execute("<remote_scheduler.start_remote_task tcp_port='WRONG'/>") }
    intercept[CommandException] { execute("<remote_scheduler.start_remote_task tcp_port='999'/>") }
    execute("<remote_scheduler.start_remote_task tcp_port='1111'/>")
  }

  private class CommandException extends Exception
}

private object CommandXmlExecutorTest {
  private val IP = InetAddress.getByName("127.0.0.99")
  private val ASocketAddress = new InetSocketAddress(IP, 999)
  private val BSocketAddress = new InetSocketAddress(IP, 1111)
}
