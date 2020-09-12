package js7.tests

import java.nio.file.Files.{delete, move}
import java.nio.file.Paths
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.common.scalautil.FileUtils.syntax._
import js7.controller.data.events.ControllerAgentEvent.AgentCouplingFailed
import js7.core.event.journal.files.JournalFiles.listJournalFiles
import js7.data.agent.AgentRefPath
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.problems.UnknownEventIdProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CoupleControllerTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CoupleControllerTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val inventoryItems = TestWorkflow :: Nil
  override protected def controllerConfig = config"""
    js7.journal.remove-obsolete-files = false
    js7.akka.http.connection-pool-shutdown-timeout = 0s
    """.withFallback(super.controllerConfig)

  private lazy val agentStateDir = directoryProvider.agents.head.dataDir / "state"
  private val orderGenerator = Iterator.from(1).map(i => FreshOrder(OrderId(i.toString), TestWorkflow.path))

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents(0).writeExecutable(TestExecutablePath, script(0.s))
  }

  // Test does not work reliable.
  // But ControllersEventRouteTest has an equivalent test.
  if (false) "CoupleController command fails with UnknownEventIdProblem when Agent misses old events" in {
    directoryProvider.runController() { controller =>
      val firstJournalFile = agentStateDir / "controller-Controller--0.journal"
      var lastEventId = EventId.BeforeFirst
      directoryProvider.runAgents() { _ =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        lastEventId = lastEventIdOf(controller.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
      }

      // DELETE OLD AGENTS'S EVENTS THE CONTROLLER HAS NOT READ => UnknownEventIdProblem
      move(firstJournalFile, Paths.get(s"$firstJournalFile-MOVED"))
      directoryProvider.runAgents() { _ =>
        val event = controller.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate =
          ke => ke.key == agentRefPath &&
            ke.event.problem.is(UnknownEventIdProblem))
        lastEventId = event.last.eventId
      }
      move(Paths.get(s"$firstJournalFile-MOVED"), firstJournalFile)

      // LET CONTROLLER READ MORE AGENT'S EVENTS
      directoryProvider.runAgents() { _ =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        lastEventId = lastEventIdOf(controller.eventWatch.await[OrderFinished](after = lastEventId, predicate = _.key == order.id))
        // Agent takes a snapshot now, so the events of order2 are next to last journal file
      }
    }
  }

  "CoupleController command fails with UnknownEventIdProblem if Agent misses last events" in {
    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        controller.eventWatch.await[OrderFinished](predicate = _.key == order.id)
      }
    }
    // START AGENT WITH LAST JOURNAL FILE DELETED.
    // AGENT HAS AN OLDER EVENTID AS CONTROLLER HAS OBSERVED => UnknownEventIdProblem
    val journalFiles = listJournalFiles(agentStateDir / "controller-Controller").map(_.file)
    for (o <- journalFiles.init) move(o, Paths.get(s"$o-MOVED"))
    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        controller.eventWatch.await[AgentCouplingFailed](after = controller.recoveredEventId,
          predicate = ke =>
            ke.key == agentRefPath && ke.event.problem.is(UnknownEventIdProblem))
      }
      for (o <- journalFiles.init) move(Paths.get(s"$o-MOVED"), o)
    }
  }

  "CoupleController command fails with UnknownEventIdProblem if Agent restarts without journal" in {
    directoryProvider.runController() { controller =>
      directoryProvider.runAgents() { _ =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        controller.eventWatch.await[OrderFinished](predicate = _.key == order.id)
      }
      // Delete Agent journal
      listJournalFiles(agentStateDir / "controller-Controller").map(_.file) foreach delete
      directoryProvider.runAgents() { _ =>
        controller.eventWatch.await[AgentCouplingFailed](after = controller.recoveredEventId,
          predicate = ke =>
            ke.key == agentRefPath && ke.event.problem.is(UnknownEventIdProblem))
      }
    }
  }
}

private object CoupleControllerTest
{
  private val agentRefPath = AgentRefPath("/AGENT-111")
  private val TestExecutablePath = ExecutablePath("/TEST.cmd")

  private val TestWorkflow = Workflow(WorkflowPath("/test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentRefPath, TestExecutablePath))))

  private def lastEventIdOf[E <: Event](stamped: IterableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.iterator.to(Iterable).last.eventId
}
