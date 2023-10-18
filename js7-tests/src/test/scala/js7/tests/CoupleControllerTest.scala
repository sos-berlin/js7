package js7.tests

import java.nio.file.Files.{delete, move}
import java.nio.file.Paths
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentReady, AgentShutDown}
import js7.data.agent.{AgentPath, AgentRefState}
import js7.data.delegate.DelegateCouplingState
import js7.data.event.{Event, EventId, KeyedEvent, Stamped}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.problems.UnknownEventIdProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.CoupleControllerTest.*
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class CoupleControllerTest extends OurTestSuite with DirectoryProviderForScalaTest:
  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(TestWorkflow)

  override protected def controllerConfig = config"""
    js7.journal.remove-obsolete-files = false
    js7.pekko.http.connection-pool-shutdown-timeout = 0s
    """.withFallback(super.controllerConfig)

  override protected def agentConfig = config"""
    js7.journal.remove-obsolete-files = false
    """.withFallback(super.agentConfig)

  private lazy val agentStateDir = directoryProvider.agentEnvs.head.dataDir / "state"
  private val orderGenerator = Iterator.from(1).map(i => FreshOrder(OrderId(i.toString), TestWorkflow.path))

  override def beforeAll() =
    super.beforeAll()
    directoryProvider.agentEnvs(0).writeExecutable(TestPathExecutable, script(0.s))

  // Test does not work reliable.
  // But EventRouteTest has an equivalent test.
  if false then "CoupleController command fails with UnknownEventIdProblem when Agent misses old events" in:
    directoryProvider.runController() { controller =>
      val firstJournalFile = agentStateDir / "agent--0.journal"
      var lastEventId = EventId.BeforeFirst
      directoryProvider.runAgents() { _ =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        lastEventId = lastEventIdOf(controller.eventWatch.await[OrderFinished](
          after = lastEventId,
          predicate = _.key == order.id))
      }

      controller.eventWatch.await[AgentShutDown](after = lastEventId)
      assert(controller.controllerState().keyTo(AgentRefState)(agentPath).couplingState ==
        DelegateCouplingState.ShutDown)

      // DELETE OLD AGENTS'S EVENTS THE CONTROLLER HAS NOT READ => UnknownEventIdProblem
      move(firstJournalFile, Paths.get(s"$firstJournalFile-MOVED"))
      directoryProvider.runAgents() { _ =>
        val event = controller.eventWatch.await[AgentCouplingFailed](after = lastEventId, predicate =
          ke => ke.key == agentPath &&
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

  "CoupleController command fails with UnknownEventIdProblem if Agent misses last events" in:
    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        controller.eventWatch.await[AgentReady](after = controller.recoveredEventId)
      }
    }
    def agentJournalFiles() = listJournalFiles(agentStateDir / "agent").map(_.file)
    val firstAgentJournalFiles = agentJournalFiles()
    assert(firstAgentJournalFiles.size == 2)
    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        controller.eventWatch.await[AgentReady](after = controller.recoveredEventId)
      }
    }
    // Agent has written a snapshot when terminated, incrementing the EventId without notifiying the COntroller
    // START AGENT WITH LAST JOURNAL FILES DELETED.
    // AGENT HAS AN OLDER EVENTID AS CONTROLLER HAS OBSERVED => UnknownEventIdProblem
    val lastAgentJournalFiles = agentJournalFiles() filterNot firstAgentJournalFiles.toSet
    assert(lastAgentJournalFiles.size == 2)
    for o <- firstAgentJournalFiles do logger.info(s"KEEP ${o.getFileName}")
    for o <- lastAgentJournalFiles do
      logger.info(s"💥 DELETE ${o.getFileName} 💥")
      move(o, Paths.get(s"$o-MOVED"))

    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        controller.eventWatch.await[AgentCouplingFailed](after = controller.recoveredEventId,
          predicate = ke =>
            ke.key == agentPath && ke.event.problem.is(UnknownEventIdProblem))
      }
    }

    // Delete newly written journal files (written after tail has been removed)
    for o <- agentJournalFiles().filterNot(firstAgentJournalFiles.toSet) do delete(o)

    // Restore original journal files
    for o <- lastAgentJournalFiles do move(Paths.get(s"$o-MOVED"), o)

  "CoupleController succeeds again if Agent's journal files are restored" in:
    directoryProvider.runAgents() { _ =>
      directoryProvider.runController() { controller =>
        controller.eventWatch.await[AgentShutDown](after = controller.recoveredEventId)
        controller.eventWatch.await[AgentReady](after = controller.recoveredEventId)
      }
    }

  "CoupleController command fails with UnknownEventIdProblem if the agent journal's tail has been deleted" in:
    directoryProvider.runController() { controller =>
      directoryProvider.runAgents() { _ =>
        val order = orderGenerator.next()
        controller.addOrderBlocking(order)
        controller.eventWatch.await[OrderFinished](predicate = _.key == order.id)
      }
      // Delete the last two agent journal files
      listJournalFiles(agentStateDir / "agent").takeRight(2).map(_.file) foreach delete
      directoryProvider.runAgents() { _ =>
        controller.eventWatch.await[AgentCouplingFailed](after = controller.recoveredEventId,
          predicate = ke =>
            ke.key == agentPath && ke.event.problem.is(UnknownEventIdProblem))
      }
    }

private object CoupleControllerTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val TestPathExecutable = RelativePathExecutable("TEST.cmd")

  private val TestWorkflow = Workflow(WorkflowPath("test") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentPath, TestPathExecutable))))

  private def lastEventIdOf[E <: Event](stamped: IterableOnce[Stamped[KeyedEvent[E]]]): EventId =
    stamped.iterator.to(Iterable).last.eventId
