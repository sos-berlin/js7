package js7.tests.controller

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.TakeSnapshot
import js7.data.controller.ControllerEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.controller.ObsoleteJournalFilesRemovedTest.*
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class ObsoleteJournalFilesRemovedTest extends OurTestSuite with DirectoryProviderForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(workflow)
  override protected def controllerConfig = config"js7.journal.release-events-delay = 0s"
    .withFallback(super.controllerConfig)


  "Obsolete journal files are removed if nothing has been configured" in {
    for ((_, tree) <- directoryProvider.agentToTree) {
      tree.writeExecutable(pathExecutable, script(0.s))
    }

    def controllerJournalFiles = listJournalFiles(directoryProvider.controller.dataDir / "state" / "controller")

    directoryProvider.run { (controller, _) =>
      controller.runOrder(aOrder)
    }
    assert(controllerJournalFiles.size == 1)

    directoryProvider.run { case (controller, _) =>
      controller.eventWatch.await[ControllerEvent.ControllerReady]()
      assert(controllerJournalFiles.size == 1)

      controller.api.executeCommand(TakeSnapshot).await(99.s).orThrow
      assert(controllerJournalFiles.size == 1)
    }
  }
}

private object ObsoleteJournalFilesRemovedTest
{
  private val agentPath = AgentPath("agent-111")
  private val pathExecutable = RelativePathExecutable(s"TEST$sh")
  private val workflow = Workflow.of(WorkflowPath("test"),
    Execute(WorkflowJob(agentPath, pathExecutable)))
  private val aOrder = FreshOrder(OrderId("🔷"), workflow.id.path)
}
