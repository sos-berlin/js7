package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.client.main.AgentClientMain
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.agent.test.TestAgentProvider
import com.sos.jobscheduler.agent.tests.AgentClientMainTest._
import com.sos.jobscheduler.common.guice.ScalaAbstractModule
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class AgentClientMainTest extends FreeSpec with BeforeAndAfterAll with HasCloser with TestAgentProvider {

  override def afterAll() = closer closeThen super.afterAll()

  override protected def extraAgentModule = new ScalaAbstractModule {
    override def configure() = {
      bindInstance[CommandHandler](new CommandHandler {
        def execute(command: AgentCommand, meta: CommandMeta): Future[command.Response] = {
          val response = command match {
            case ExpectedTerminate ⇒ AgentCommand.Accepted
            case _ ⇒ fail()
          }
          Future.successful(response.asInstanceOf[command.Response])
        }

        def overview = throw new NotImplementedError
        def detailed = throw new NotImplementedError
      })
    }
  }

  "main" in {
    val output = mutable.Buffer[String]()
    val commandYaml = """{ TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }"""
    AgentClientMain.run(List(agent.localUri.toString, commandYaml, "?"), o ⇒ output += o)
    assert(output.size == 3)
    assert(output(0) == "TYPE: Accepted")
    assert(output(1) == "---")
    assert(output(2) contains "startedAt: '2")
    assert(output(2) contains "isTerminating: false")
  }

  "main with Agent URI only checks wether Agent is responding (it is)" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      AgentClientMain.run(List(agent.localUri.toString), o ⇒ output += o)
    }
    assert(output == List("JobScheduler Agent is responding"))
  }

  "main with Agent URI only checks wether Agent is responding (it is not)" in {
    val port = findRandomFreeTcpPort()
    val output = mutable.Buffer[String]()
    assertResult(1) {
      AgentClientMain.run(List(s"http://127.0.0.1:$port"), output += _)
    }
    assert(output.head contains "JobScheduler Agent is not responding: ")
    assert(output.head contains "Connection refused")
  }
}

private object AgentClientMainTest {
  private val ExpectedTerminate = Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(10.seconds))
}

