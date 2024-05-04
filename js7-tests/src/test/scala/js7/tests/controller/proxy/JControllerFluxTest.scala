package js7.tests.controller.proxy

import js7.base.test.OurTestSuite
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentPath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.tests.controller.proxy.JControllerFluxTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.jdk.CollectionConverters.*

final class JControllerFluxTest extends OurTestSuite with ControllerAgentForScalaTest:

  protected def agentPaths = Seq(AgentPath("AGENT"))
  protected def items = Seq(workflow)

  "JControllerProxy#flux is switchable to an own Executor" in:
    val tester = new JControllerFluxTester(
      List(JAdmission(controllerAdmission)).asJava,
      JHttpsConfig.empty)
    autoClosing(tester) { _ =>
      tester.test1 { () =>
        // Should be executed in the ForkJoinPool.commonPool, as requested by test1.
        assert(Thread.currentThread.getName.startsWith("ForkJoinPool.commonPool-worker-"))
      }
    }

object JControllerFluxTest:

  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Nil)
