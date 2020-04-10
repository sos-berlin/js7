package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.problems.UnknownEventIdProblem
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import com.sos.jobscheduler.tests.CoupleMasterTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import java.nio.file.Files.move
import java.nio.file.Paths
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CoupleMasterTest extends FreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = TestWorkflow :: Nil

  private lazy val agentStateDir = directoryProvider.agents.head.dataDir / "state"
  private lazy val firstJournalFile = agentStateDir / "master-Master--0.journal"
  private val orderGenerator = Iterator.from(1).map(i => FreshOrder(OrderId(i.toString), TestWorkflow.path))
  private var lastEventId = EventId.BeforeFirst

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents(0).writeExecutable(TestExecutablePath, script(0.s))
  }

  "CoupleMaster command fails with UnknownEventIdProblem when Agent misses old events" in {
    directoryProvider.runMaster() { master =>
      directoryProvider.runAgents() { _ =>
        val order = orderGenerator.next()
        master.addOrderBlocking(order)
        lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
      }

      // DELETE OLD AGENTS'S EVENTS THE MASTER HAS NOT READ => UnknownEventIdProblem
      move(firstJournalFile, Paths.get(s"$firstJournalFile-MOVED"))
      directoryProvider.runAgents() { _ =>
        val event = master.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate =
          ke => ke.key == agentRefPath &&
            ke.event.problem.codeOption.contains(UnknownEventIdProblem.code))
        lastEventId = event.last.eventId
      }
      move(Paths.get(s"$firstJournalFile-MOVED"), firstJournalFile)

      // LET MASTER READ MORE AGENT'S EVENTS
      directoryProvider.runAgents() { _ =>
        val order = orderGenerator.next()
        master.addOrderBlocking(order)
        lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
        // Agent takes a snapshot now, so the events of order2 are next to last journal file
      }
    }
  }

  "CoupleMaster command fails with UnknownEventIdProblem if Agent misses last events" in {
    directoryProvider.runMaster() { master =>
      // REMOVE NEW AGENTS'S EVENTS THE MASTER HAS ALREADY READ => UnknownEventIdProblem
      val journalFiles = agentStateDir.pathSet.toVector
        .map(_.getFileName.toString)
        .filter(_.startsWith("master-Master--"))
        .sorted
        .map(agentStateDir / _)
      for (o <- journalFiles.init) move(o, Paths.get(s"$o-MOVED"))
      directoryProvider.runAgents() { _ =>
        master.eventWatch.await[AgentCouplingFailed](after = master.eventWatch.lastFileTornEventId, predicate =
          ke => ke.key == agentRefPath &&
            ke.event.problem.codeOption.contains(UnknownEventIdProblem.code))
      }
    }
  }
}

private object CoupleMasterTest
{
  private val agentRefPath = AgentRefPath("/AGENT-111")
  private val TestExecutablePath = ExecutablePath("/TEST.cmd")

  private val TestWorkflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentRefPath, TestExecutablePath))))

  private def lastEventIdOf[E <: Event](stamped: IterableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.iterator.to(Iterable).last.eventId
}
