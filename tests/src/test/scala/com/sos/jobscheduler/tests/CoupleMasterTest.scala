package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.problems.MasterRequiresUnknownEventIdProblem
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import com.sos.jobscheduler.tests.CoupleMasterTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import java.nio.file.Files.{copy, move}
import java.nio.file.Paths
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class CoupleMasterTest extends FreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = TestWorkflow :: Nil

  private lazy val stateDir = directoryProvider.agents.head.data / "state"
  private lazy val firstJournalFile = stateDir / "master-Master--0.journal"
  private lazy val savedFirstJournalFile = stateDir / "SAVED-master-Master--0.journal"
  private val Seq(order1, order2) = for (i <- 1 to 2) yield FreshOrder(OrderId(i.toString), TestWorkflow.path)
  private var lastEventId = EventId.BeforeFirst

  "CoupleMaster command fails with MasterRequiresUnknownEventIdProblem when Agent misses old events" in {
    directoryProvider.runMaster() { master =>
      directoryProvider.agents(0).writeExecutable(TestExecutablePath, script(0.s))
      directoryProvider.runAgents() { _ =>
        master.addOrderBlocking(order1)
        lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order1.id))
        copy(firstJournalFile, savedFirstJournalFile)
      }

      // DELETE OLD AGENTS'S EVENTS THE MASTER HAS NOT READ => MasterRequiresUnknownEventIdProblem
      move(firstJournalFile, Paths.get(s"$firstJournalFile-MOVED"))
      directoryProvider.runAgents() { _ =>
        val event = master.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate =
          ke => ke.key == agentRefPath &&
            ke.event.problem.codeOption.contains(MasterRequiresUnknownEventIdProblem.code))
        lastEventId = event.last.eventId
      }
      move(Paths.get(s"$firstJournalFile-MOVED"), firstJournalFile)

      // LET MASTER READ MORE AGENT'S EVENTS
      directoryProvider.runAgents() { _ =>
        master.addOrderBlocking(order2)
        lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order2.id))
        // Agent takes a snapshot now, so the events of order2 are next to last journal file
      }
    }
  }

  "CoupleMaster command fails with MasterRequiresUnknownEventIdProblem if Agent misses last events" in {
    directoryProvider.runMaster() { master =>
      // DELETE NEW AGENTS'S EVENTS THE MASTER HAS ALREADY READ => MasterRequiresUnknownEventIdProblem
      val lastJournalFiles = stateDir.pathSet.toVector
        .map(_.getFileName.getPath)
        .filter(_ startsWith "master-Master--")
        //.sorted.takeRight(2)  // The last journal file contains the shutdown snapshot, the next to last file contains the events
        .map(stateDir / _)
      for (o <- lastJournalFiles) move(o, Paths.get(s"$o-MOVED"))
      move(savedFirstJournalFile, firstJournalFile, ATOMIC_MOVE)
      directoryProvider.runAgents() { _ =>
        master.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate =
          ke => ke.key == agentRefPath &&
            ke.event.problem.codeOption.contains(MasterRequiresUnknownEventIdProblem.code))
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

  private def lastEventIdOf[E <: Event](stamped: TraversableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.toVector.last.eventId
}
