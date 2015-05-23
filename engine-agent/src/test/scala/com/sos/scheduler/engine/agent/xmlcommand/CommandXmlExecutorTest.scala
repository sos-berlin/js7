package com.sos.scheduler.engine.agent.xmlcommand

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands.{Command, StartProcessResponse, StartSeparateProcess, StartThread}
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutor.throwableToString
import com.sos.scheduler.engine.agent.xmlcommand.CommandXmlExecutorTest._
import com.sos.scheduler.engine.common.scalautil.Futures._
import org.junit.runner.RunWith
import org.scalatest.Assertions.fail
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
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
    intercept[CommandException] { executeCommand("INVALID XML") }
    intercept[CommandException] { executeCommand("<WRONG/>") }
    intercept[CommandException] { executeCommand(s"<remote_scheduler.start_remote_task ip_address='$IP' tcp_port='WRONG' kind='process'/>") }
    intercept[CommandException] { executeCommand(s"<remote_scheduler.start_remote_task ip_address='$IP' tcp_port='999' kind='process'/>") }
    executeCommand(s"<remote_scheduler.start_remote_task ip_address='$IP' tcp_port='1111' kind='process'/>") shouldEqual
      <spooler><answer><process process_id="111"/></answer></spooler>
    executeCommand(s"<remote_scheduler.start_remote_task ip_address='$IP' tcp_port='1111' java_options='OPTIONS' java_classpath='CLASSPATH'/>") shouldEqual
      <spooler><answer><process process_id="222"/></answer></spooler>
  }

  "throwableToString repairs Scala 2.11.5 'Boxed Error' message" in {
    throwableToString(new Exception("TEXT")) shouldEqual "java.lang.Exception: TEXT"
    val throwable = Await.ready(Future { throw new Error("TEXT") }, 10.seconds).value.get.failed.get
    throwable.toString shouldEqual "java.util.concurrent.ExecutionException: Boxed Error"  // "TEXT" is lost
    throwableToString(throwable) shouldEqual "java.lang.Error: TEXT"
  }
}

private object CommandXmlExecutorTest {
  private val IP = "127.0.0.99"
  private val ASocketAddress = s"$IP:999"
  private val BSocketAddress = s"$IP:1111"

  private def executeCommand(command: String): xml.Elem = {
    val executed = CommandXmlExecutor.execute(command)(executeCommand)
    awaitResult(executed, 10.seconds) match {
      case <spooler><answer>{elem: xml.Elem}</answer></spooler> if elem.label == "ERROR" ⇒ throw new CommandException
      case o ⇒ o
    }
  }

  private def executeCommand(command: Command) = command match {
    case StartThread(ASocketAddress) ⇒ Future { throw new Exception }
    case StartThread(BSocketAddress) ⇒ Future { StartProcessResponse(AgentProcessId(111)) }
    case StartSeparateProcess(BSocketAddress, "OPTIONS", "CLASSPATH") ⇒ Future { StartProcessResponse(AgentProcessId(222)) }
    case o ⇒ fail(o.toString)
  }

  private class CommandException extends Exception
}
