package js7.tests.controller

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.TakeSnapshot
import js7.controller.data.events.ControllerEvent
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.data.agent.AgentName
import js7.data.job.RelativeExecutablePath
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.ObsoleteJournalFilesRemovedTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ObsoleteJournalFilesRemovedTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentNames = agentName :: Nil
  protected val inventoryItems = workflow :: Nil
  override protected def controllerConfig = config"js7.journal.release-events-delay = 0s"
    .withFallback(super.controllerConfig)


  "Obsolete journal files are removed if nothing has been configured" in {
    for ((_, tree) <- directoryProvider.agentToTree) {
      tree.writeExecutable(executablePath, script(0.s))
    }

    def controllerJournalFiles = listJournalFiles(directoryProvider.controller.dataDir / "state" / "controller")

    directoryProvider.run { (controller, _) =>
      controller.runOrder(aOrder)
    }
    assert(controllerJournalFiles.size == 1)

    directoryProvider.run { case (controller, _) =>
      controller.eventWatch.await[ControllerEvent.ControllerReady]()
      assert(controllerJournalFiles.size == 1)

      controller.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assert(controllerJournalFiles.size == 1)
    }
  }
}

private object ObsoleteJournalFilesRemovedTest
{
  private val agentName = AgentName("agent-111")
  private val executablePath = RelativeExecutablePath(s"TEST$sh")
  private val workflow = Workflow.of(WorkflowPath("/test"),
    Execute(WorkflowJob(agentName, executablePath)))
  private val aOrder = FreshOrder(OrderId("🔵"), workflow.id.path)
}
