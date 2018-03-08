package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{SourceType, VersionId}
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderStdoutWritten}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand.ReadConfigurationDirectory
import com.sos.jobscheduler.master.tests.TestEventCollector
import java.nio.file.Files.delete
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import scala.util.Try

final class ConfigurationTest extends FreeSpec {
  import ConfigurationTest._

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      val eventCollector = new TestEventCollector
      for (v ← 1 to 4)  // For each version, we use a dedicated job which echos the VersionId
        directoryProvider.agents.head.file(JobPath(s"/JOB-V$v"), SourceType.Xml).xml =
          <job><script language="shell">echo /VERSION-{v.toString}/</script></job>

      directoryProvider.runAgents { _ ⇒
        directoryProvider.runMaster { master ⇒
          eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])

          addWorkflowAndRunOrder(master, V1, AWorkflowPath, OrderId("A"))
          // Command is rejected due to duplicate VersionId
          assert(Try { master.executeCommand(ReadConfigurationDirectory(V1)) await 99.s }
            .failed.get.getMessage contains s"Duplicate VersionId '${V1.string}'")

          addWorkflowAndRunOrder(master, V2, BWorkflowPath, OrderId("B"))
        }
        // Recovery
        directoryProvider.runMaster { master ⇒
          // V2
          eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
          // Previously defined workflow is still known
          runOrder(master, BWorkflowPath % V2, OrderId("B-AGAIN"))

          // V3 - Add and use a new workflow
          addWorkflowAndRunOrder(master, V3, CWorkflowPath, OrderId("C"))

          // Handling a workflow change is not implemented
          directoryProvider.master.writeJson(testWorkflow(V4) withId CWorkflowPath % VersionId.Anonymous)
          assert(Try { master.executeCommand(ReadConfigurationDirectory(V4)) await 99.s }
            .failed.get.toStringWithCauses contains "Change of configuration file is not supported: Workflow:/C")

          // Handling a workflow deletion is not implemented
          delete(directoryProvider.master.file(CWorkflowPath, SourceType.Json))
          assert(Try { master.executeCommand(ReadConfigurationDirectory(V4)) await 99.s }
            .failed.get.toStringWithCauses contains "Deletion of configuration file is not supported: Workflow:/C")

          // Command is rejected due to duplicate VersionId
          assert(Try { master.executeCommand(ReadConfigurationDirectory(V2)) await 99.s }
            .failed.get.getMessage contains s"Duplicate VersionId '${V2.string}'")

          // AWorkflowPath is still version V1
          runOrder(master, AWorkflowPath % V1, OrderId("A-2"))
          runOrder(master, BWorkflowPath % V2, OrderId("B-2"))
        }
      }

      def addWorkflowAndRunOrder(master: RunningMaster, versionId: VersionId, path: WorkflowPath, orderId: OrderId): Unit = {
        val workflow = testWorkflow(versionId)
        assert(workflow.isAnonymous)
        val order = FreshOrder(orderId, path)
        // Command will be rejected because workflow is not yet defined
        assert(Try { master.addOrder(order) await 99.s }
          .failed.get.getMessage contains s"No such key 'Workflow:${path.string}'")
        // Add Workflow
        directoryProvider.master.writeJson(workflow withId path % VersionId.Anonymous)
        master.executeCommand(ReadConfigurationDirectory(versionId)) await 99.s
        master.addOrder(order) await 99.s
        awaitOrder(order.id, path % versionId)
      }

      def runOrder(master: RunningMaster, workflowId: WorkflowId, orderId: OrderId): Unit = {
        val order = FreshOrder(orderId, workflowId.path)
        master.addOrder(order) await 99.s
        awaitOrder(orderId, workflowId)
      }

      def awaitOrder(orderId: OrderId, workflowId: WorkflowId): Unit = {
        val orderAdded: OrderAdded = eventCollector.await[OrderAdded](_.key == orderId).head.value.event
        assert(orderAdded.workflowId == workflowId)
        val written = eventCollector.await[OrderStdoutWritten](_.key == orderId).head.value.event
        assert(written.chunk contains s"/VERSION-${workflowId.versionId.string}/")
        eventCollector.await[OrderFinished](_.key == orderId)
      }
    }
  }
}

object ConfigurationTest {
  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val V4 = VersionId("4")
  private val TestAgentPath = AgentPath("/AGENT")

  private def testWorkflow(versionId: VersionId) = Workflow.of(Job(JobPath(s"/JOB-V${versionId.string}"), TestAgentPath))

  private implicit class WithVersionWorkflow(private val underlying: Workflow) extends AnyVal {
    def withVersion(versionId: VersionId): Workflow =
      underlying.copy(id = underlying.id.copy(versionId = versionId))
  }
}
