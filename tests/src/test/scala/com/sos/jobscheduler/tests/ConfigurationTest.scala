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
import com.sos.jobscheduler.data.filebased.{FileBasedVersion, SourceType}
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{JobPath, Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand.ReadConfigurationDirectory
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.ConfigurationTest._
import java.nio.file.Files.delete
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import scala.util.Try

final class ConfigurationTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      val eventCollector = new TestEventCollector
      for (a ← directoryProvider.agents) a.file(JobPath("/JOB"), SourceType.Xml).xml = <job><script language="shell">:</script></job>

      directoryProvider.runAgents { _ ⇒
        directoryProvider.runMaster { master ⇒
          eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
          testAddWorkflow(master, ANamedWorkflow, OrderId("A"), Some(FileBasedVersion("0.1")))
          // Command is rejected due to duplicate version identifier
          assert(Try { master.executeCommand(ReadConfigurationDirectory(FileBasedVersion("0.1"))) await 99.s }
            .failed.get.getMessage contains "Duplicate version '0.1'")
          testAddWorkflow(master, BNamedWorkflow, OrderId("B"), Some(FileBasedVersion("0.2")))
        }
        // Recovery
        directoryProvider.runMaster { master ⇒
          eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
          // Previously defined workflow is still known
          testAddWorkflow(master, BNamedWorkflow, OrderId("B-2"), None)
          // Add and use a new workflow
          testAddWorkflow(master, CNamedWorkflow, OrderId("C"), Some(FileBasedVersion("0.3")))

          // Handling a workflow change is not implemented
          directoryProvider.master.writeJson(C2NamedWorkflow.path, C2NamedWorkflow.workflow)
          assert(Try { master.executeCommand(ReadConfigurationDirectory(FileBasedVersion("0.4"))) await 99.s }
            .failed.get.toStringWithCauses contains "Change of configuration file is not supported: Workflow:/C")

          // Handling a workflow deletion is not implemented
          delete(directoryProvider.master.file(CNamedWorkflow.path, SourceType.Json))
          assert(Try { master.executeCommand(ReadConfigurationDirectory(FileBasedVersion("0.4"))) await 99.s }
            .failed.get.toStringWithCauses contains "Deletion of configuration file is not supported: Workflow:/C")
        }
      }

      def testAddWorkflow(master: RunningMaster, namedWorkflow: Workflow.Named, orderId: OrderId, version: Option[FileBasedVersion]): Unit = {
        val order = Order(orderId, namedWorkflow.path, Order.StartNow)
        for (v ← version) {
          // Command will be rejected because workflow is not yet defined
          assert(Try { master.addOrder(order) await 99.s }
            .failed.get.getMessage contains s"No such key 'Workflow:${namedWorkflow.path.string}'")
          // Add Workflow
          directoryProvider.master.writeJson(namedWorkflow.path, namedWorkflow.workflow)
          master.executeCommand(ReadConfigurationDirectory(v)) await 99.s
        }
        master.addOrder(order) await 99.s
        eventCollector.await[OrderFinished](_.key == order.id)
      }
    }
  }
}

object ConfigurationTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestWorkflow = Workflow.of(Job(JobPath("/JOB"), TestAgentPath))
  private val ANamedWorkflow = Workflow.Named(WorkflowPath("/A"), TestWorkflow)
  private val BNamedWorkflow = Workflow.Named(WorkflowPath("/B"), ANamedWorkflow.workflow)
  private val CNamedWorkflow = Workflow.Named(WorkflowPath("/C"), ANamedWorkflow.workflow)
  private val C2NamedWorkflow = Workflow.Named(WorkflowPath("/C"), Workflow.of(Job(JobPath("/JOB-2"), TestAgentPath)))
}
