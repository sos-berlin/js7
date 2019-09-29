package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.event.journal.data.JournalHeader.JournalIdMismatchProblem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import com.sos.jobscheduler.tests.JournalIdTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import io.circe.syntax.EncoderOps
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalIdTest extends FreeSpec with DirectoryProviderForScalaTest
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

  "CoupleMaster fails if Agent's historic journal file has an alien JournalId" in {
    directoryProvider.runAgents() { _ =>
      directoryProvider.runMaster() { master =>
        val order = orderGenerator.next()
        master.addOrderBlocking(order)
        lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
      }
    }

    val firstJournalFileContent = firstJournalFile.contentString
    locally {
      val (headerLine, body) = firstJournalFileContent.splitAt(firstJournalFileContent indexOf '\n')
      val header = headerLine.parseJsonCheckedAs[JournalHeader].orThrow
      firstJournalFile := header.copy(journalId = JournalId.random()).asJson.compactPrint + body
    }

    directoryProvider.runAgents() { _ =>
      directoryProvider.runMaster() { master =>
        master.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate = ke =>
          ke.key == agentRefPath &&
            //ke.event.problem.codeOption.contains(JournalIdMismatchProblem.code))   -- It's thrown as ProblemException
            ke.event.problem.toString.contains(JournalIdMismatchProblem.code.string))
      }
    }

    /*
    // FETCHING EVENTS FAILS IF AGENT'S HISTORIC JOURNAL FILE HAS AN ALIEN JOURNALID
    // Restore first journal file
    firstJournalFile := firstJournalFileContent

    // Make another journal file and let Master start fetching its content
    directoryProvider.runAgents() { _ =>
      directoryProvider.runMaster() { master =>
        val order = orderGenerator.next()
        master.addOrderBlocking(order)
        lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
      }
    }

    locally {
      val secondJournalFile = agentStateDir.pathSet
        .filter(o => o.getFileName.toString.startsWith("master-Master--") && o != firstJournalFile).min
      println(s"### $secondJournalFile")
      val secondJournalFileContent = secondJournalFile.contentString
      val (headerLine, body) = secondJournalFileContent.splitAt(secondJournalFileContent indexOf '\n')
      val header = headerLine.parseJsonCheckedAs[JournalHeader].orThrow
      secondJournalFile := header.copy(journalId = JournalId.random()).asJson.compactPrint + body
    }

    directoryProvider.runAgents() { _ =>
      directoryProvider.runMaster() { master =>
        val order = orderGenerator.next()
        master.addOrderBlocking(order)
        lastEventId = lastEventIdOf(master.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
        //master.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate = ke =>
        //  ke.key == agentRefPath &&
        //    ke.event.problem.codeOption.contains(JournalIdMismatchProblem.code))
      }
    }
    */
  }
}

private object JournalIdTest
{
  private val agentRefPath = AgentRefPath("/AGENT-111")
  private val TestExecutablePath = ExecutablePath("/TEST.cmd")

  private val TestWorkflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentRefPath, TestExecutablePath))))

  private def lastEventIdOf[E <: Event](stamped: TraversableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.toVector.last.eventId
}
