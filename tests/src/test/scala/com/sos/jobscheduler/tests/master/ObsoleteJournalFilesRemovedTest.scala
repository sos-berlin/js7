package js7.tests.master

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.data.MasterCommand.TakeSnapshot
import js7.master.data.events.MasterEvent
import js7.tests.master.ObsoleteJournalFilesRemovedTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ObsoleteJournalFilesRemovedTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = workflow :: Nil

  "Obsolete journal files are removed if nothing has been configured" in {
    for ((_, tree) <- directoryProvider.agentToTree) {
      tree.writeExecutable(executablePath, script(0.s))
    }

    def masterJournalFiles = listJournalFiles(directoryProvider.master.dataDir / "state" / "master")

    directoryProvider.run { (master, _) =>
      master.runOrder(aOrder)
    }
    assert(masterJournalFiles.size == 1)

    directoryProvider.run { case (master, _) =>
      master.eventWatch.await[MasterEvent.MasterReady]()
      assert(masterJournalFiles.size == 1)

      master.executeCommandAsSystemUser(TakeSnapshot).await(99.s).orThrow
      assert(masterJournalFiles.size == 1)
    }
  }
}

private object ObsoleteJournalFilesRemovedTest
{
  private val agentRefPath = AgentRefPath("/agent-111")
  private val executablePath = ExecutablePath(s"/TEST$sh")
  private val workflow = Workflow.of(WorkflowPath("/test"),
    Execute(WorkflowJob(agentRefPath, executablePath)))
  private val aOrder = FreshOrder(OrderId("🔵"), workflow.id.path)
}
