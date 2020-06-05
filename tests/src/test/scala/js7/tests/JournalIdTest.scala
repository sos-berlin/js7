package js7.tests

import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.data.agent.AgentRefPath
import js7.data.event.JournalHeader.JournalIdMismatchProblem
import js7.data.event.{Event, EventId, JournalHeader, JournalId, KeyedEvent, Stamped}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.data.events.MasterAgentEvent.AgentCouplingFailed
import js7.tests.JournalIdTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import com.typesafe.config.ConfigFactory
import io.circe.syntax.EncoderOps
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalIdTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val fileBased = TestWorkflow :: Nil
  override protected val masterConfig = ConfigFactory.parseString(
     """js7.journal.remove-obsolete-files = false
       |""".stripMargin)
  override protected val agentConfig = ConfigFactory.parseString(
     """js7.journal.remove-obsolete-files = false
       |""".stripMargin)

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

  private def lastEventIdOf[E <: Event](stamped: IterableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.iterator.to(Iterable).last.eventId
}
