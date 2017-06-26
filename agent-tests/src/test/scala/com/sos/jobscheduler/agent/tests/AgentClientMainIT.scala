package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.client.main.AgentClientMain
import com.sos.jobscheduler.agent.command.{CommandExecutor, CommandMeta}
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.agent.test.AgentTest
import com.sos.jobscheduler.agent.tests.AgentClientMainIT._
import com.sos.jobscheduler.common.guice.ScalaAbstractModule
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class AgentClientMainIT extends FreeSpec with BeforeAndAfterAll with HasCloser with AgentTest {

  override def afterAll() = closer.closeThen { super.afterAll() }

  override protected def extraAgentModule = new ScalaAbstractModule {
    def configure() = {
      bindInstance[CommandExecutor](new CommandExecutor {
        def executeCommand(command: AgentCommand, meta: CommandMeta): Future[command.Response] = {
          val response = command match {
            case ExpectedTerminate ⇒ EmptyResponse
            case _ ⇒ fail()
          }
          Future.successful(response.asInstanceOf[command.Response])
        }
      })
    }
  }

  "main" in {
    val output = mutable.Buffer[String]()
    val commandYaml = """{ $TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }"""
    AgentClientMain.run(List(agent.localUri.string, commandYaml, "/"), o ⇒ output += o)
    assert(output.size == 3)
    assert(output(0) == "{}")
    assert(output(1) == "---")
    assert(output(2) contains "startedAt: '2")
    assert(output(2) contains "isTerminating: false")
    assert(output(2) contains "totalTaskCount: 0")
    assert(output(2) contains "currentTaskCount: 0")
  }

  "main with Agent URI only checks wether Agent is responding (it is)" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      AgentClientMain.run(List(agent.localUri.string), o ⇒ output += o)
    }
    assert(output == List("JobScheduler Agent is responding"))
  }

  "main with Agent URI only checks wether Agent is responding (it is not)" in {
    val port = findRandomFreeTcpPort()
    val output = mutable.Buffer[String]()
    assertResult(1) {
      AgentClientMain.run(List(s"http://127.0.0.1:$port"), output += _)
    }
    assert(output == List(s"JobScheduler Agent is not responding: Connection attempt to 127.0.0.1:$port failed"))
  }
}

private object AgentClientMainIT {
  private val ExpectedTerminate = Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(10.s))
}
