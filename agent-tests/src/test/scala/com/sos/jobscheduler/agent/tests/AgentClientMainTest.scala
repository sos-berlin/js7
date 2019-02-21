package com.sos.jobscheduler.agent.tests

import cats.data.Validated.Valid
import com.sos.jobscheduler.agent.client.main.AgentClientMain
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Terminate
import com.sos.jobscheduler.agent.test.TestAgentProvider
import com.sos.jobscheduler.agent.tests.AgentClientMainTest._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.guice.ScalaAbstractModule
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.core.command.CommandMeta
import monix.eval.Task
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class AgentClientMainTest extends FreeSpec with BeforeAndAfterAll with HasCloser with TestAgentProvider {

  override def afterAll() = closer closeThen super.afterAll()

  override protected def extraAgentModule = new ScalaAbstractModule {
    override def configure() = {
      bindInstance[CommandHandler](new CommandHandler {
        def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
          Task {
            (command match {
              case ExpectedTerminate ⇒ Valid(AgentCommand.Response.Accepted)
              case _ ⇒ fail()
            })
            .map(_.asInstanceOf[command.Response])
          }

        def overview = throw new NotImplementedError
        def detailed = throw new NotImplementedError
      })
    }
  }

  "main" in {
    val output = mutable.Buffer[String]()
    val commandYaml = """{ TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }"""
    AgentClientMain.run(List(s"-data-directory=$dataDirectory", agent.localUri.toString, commandYaml, "?"), o ⇒ output += o)
    assert(output.size == 3)
    assert(output(0) == "TYPE: Accepted")
    assert(output(1) == "---")
    assert(output(2) contains "startedAt: '2")
    assert(output(2) contains "isTerminating: false")
  }

  "main with Agent URI only checks wether Agent is responding (it is)" in {
    val output = mutable.Buffer[String]()
    assertResult(0) {
      AgentClientMain.run(List(s"-data-directory=$dataDirectory", agent.localUri.toString), o ⇒ output += o)
    }
    assert(output == List("JobScheduler Agent is responding"))
  }

  "main with Agent URI only checks wether Agent is responding (it is not)" in {
    val port = findRandomFreeTcpPort()
    val output = mutable.Buffer[String]()
    assertResult(1) {
      AgentClientMain.run(List(s"-data-directory=$dataDirectory", s"http://127.0.0.1:$port"), output += _)
    }
    assert(output.head contains "JobScheduler Agent is not responding: ")
    assert(output.head contains "Connection refused")
  }
}

private object AgentClientMainTest {
  private val ExpectedTerminate = Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(10.seconds))
}

