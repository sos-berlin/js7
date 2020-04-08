package com.sos.jobscheduler.tests.master

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.core.event.journal.files.JournalFiles.listJournalFiles
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand.TakeSnapshot
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.tests.master.ObsoleteJournalFilesRemovedTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ObsoleteJournalFilesRemovedTest extends FreeSpec with DirectoryProviderForScalaTest
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
