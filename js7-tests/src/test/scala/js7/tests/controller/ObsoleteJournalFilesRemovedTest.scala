package js7.tests.controller

import cats.effect.unsafe.IORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.TakeSnapshot
import js7.data.controller.ControllerEvent
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.controller.ObsoleteJournalFilesRemovedTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProviderForScalaTest

/**
  * @author Joacim Zschimmer
  */
final class ObsoleteJournalFilesRemovedTest extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(workflow)

  override protected def controllerConfig = config"""
    js7.journal.release-events-delay = 0s
    """

  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  "Obsolete journal files are removed if nothing has been configured" in:
    def controllerJournalFiles =
      listJournalFiles(directoryProvider.controllerEnv.dataDir / "state" / "controller")

    directoryProvider.run: (controller, _) =>
      controller.runOrder(aOrder)
    assert(controllerJournalFiles.size == 1)

    directoryProvider.run: (controller, _) =>
      controller.eventWatch.await[ControllerEvent.ControllerReady]()
      assert(controllerJournalFiles.size == 1)

      controller.api.executeCommand(TakeSnapshot).await(99.s).orThrow
      assert(controllerJournalFiles.size == 1)


private object ObsoleteJournalFilesRemovedTest:

  private val agentPath = AgentPath("agent-111")
  private val workflow = Workflow.of(WorkflowPath("test"),
    EmptyJob.execute(agentPath))
  private val aOrder = FreshOrder(OrderId("🔷"), workflow.id.path)
