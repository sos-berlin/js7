package js7.tests

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.syntax._
import js7.controller.data.events.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.AgentId
import js7.data.event.JournalHeader.JournalIdMismatchProblem
import js7.data.event.{Event, EventId, JournalHeader, JournalId, KeyedEvent, Stamped}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.JournalIdTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalIdTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val versionedItems = TestWorkflow :: Nil
  override protected val controllerConfig = config"js7.journal.remove-obsolete-files = false"
  override protected val agentConfig = config"js7.journal.remove-obsolete-files = false"

  private lazy val agentStateDir = directoryProvider.agents.head.dataDir / "state"
  private val orderGenerator = Iterator.from(1).map(i => FreshOrder(OrderId(i.toString), TestWorkflow.path))
  private var lastEventId = EventId.BeforeFirst

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents(0).writeExecutable(TestPathExecutable, script(0.s))
  }

  "CoupleController fails if Agent's historic journal file has an alien JournalId" in {
    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        lastEventId = lastEventIdOf(controller.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
      }
    }

    locally {
      directoryProvider.runAgents() { _ =>
        // Agent writes a snapshot, incrementing the EventId
      }
      val lastJournalFile = listJournalFiles(agentStateDir / "controller-Controller").last.file
      val lastJournalFileContent = lastJournalFile.contentString
      val (headerLine, body) = lastJournalFileContent.splitAt(lastJournalFileContent indexOf '\n')
      val header = headerLine.parseJsonCheckedAs[JournalHeader].orThrow
      lastJournalFile := header.copy(journalId = JournalId.random()).asJson.compactPrint + body
    }

    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        controller.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate = ke =>
          ke.key == agentId && ke.event.problem.is(JournalIdMismatchProblem))
      }
    }

    /*
    // FETCHING EVENTS FAILS IF AGENT'S HISTORIC JOURNAL FILE HAS AN ALIEN JOURNALID
    // Restore first journal file
    firstJournalFile := firstJournalFileContent

    // Make another journal file and let Controller start fetching its content
    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        lastEventId = lastEventIdOf(controller.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
      }
    }

    locally {
      val secondJournalFile = agentStateDir.pathSet
        .filter(o => o.getFileName.toString.startsWith("controller-Controller--") && o != firstJournalFile).min
      val secondJournalFileContent = secondJournalFile.contentString
      val (headerLine, body) = secondJournalFileContent.splitAt(secondJournalFileContent indexOf '\n')
      val header = headerLine.parseJsonCheckedAs[JournalHeader].orThrow
      secondJournalFile := header.copy(journalId = JournalId.random()).asJson.compactPrint + body
    }

    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        lastEventId = lastEventIdOf(controller.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
        //controller.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate = ke =>
        //  ke.key == agentId &&
        //    ke.event.problem.maybeCode.contains(JournalIdMismatchProblem.code))
      }
    }
    */
  }
}

private object JournalIdTest
{
  private val agentId = AgentId("AGENT-111")
  private val TestPathExecutable = RelativePathExecutable("TEST.cmd")

  private val TestWorkflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentId, TestPathExecutable))))

  private def lastEventIdOf[E <: Event](stamped: IterableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.iterator.to(Iterable).last.eventId
}
