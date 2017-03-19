package com.sos.jobscheduler.tests

import RecoveryIT._
import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.Terminate
import com.sos.jobscheduler.agent.scheduler.{AgentActor, AgentEvent}
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, closeOnError, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.{RichClosersAny, RichClosersAutoCloseable}
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.Master
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{JsonFileIterator, JsonJournalMeta}
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import java.util.zip.GZIPInputStream
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class RecoveryIT extends FreeSpec {

  private val eventCollector = new TestEventCollector

  "test" in {
    //while (true)
    var lastEventId = EventId.BeforeFirst
    autoClosing(new DataDirectoryProvider) { dataDirectoryProvider ⇒
      withCloser { implicit closer ⇒
        import dataDirectoryProvider.dataDir

        val agentConfs = for (name ← AgentNames) yield AgentConfiguration.forTest(Some(dataDir / name)).copy(name = name)

        (dataDir / "master/config/live/fast.job_chain.xml").xml = QuickJobChainElem
        (dataDir / "master/config/live/test.job_chain.xml").xml = TestJobChainElem
        (dataDir / "master/config/live/test.order.xml").xml = TestOrderGeneratorElem
        (dataDir / "master/config/live/test-agent-111.agent.xml").xml = <agent uri={agentConfs(0).localUri.toString}/>
        (dataDir / "master/config/live/test-agent-222.agent.xml").xml = <agent uri={agentConfs(1).localUri.toString}/>

        runMaster(dataDir) { master ⇒
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = eventCollector.oldestEventId
          }
          runAgents(agentConfs) { _ ⇒
            master.executeCommand(MasterCommand.AddOrderIfNew(FastOrder)) await 99.s
            lastEventId = lastEventIdOf(eventCollector.when[OrderEvent.OrderFinished.type](EventRequest.singleClass(after = lastEventId, 99.s), _.key == FastOrderId) await 99.s)
            lastEventId = lastEventIdOf(eventCollector.when[OrderEvent.OrderStepSucceeded](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s)
            lastEventId = lastEventIdOf(eventCollector.when[OrderEvent.OrderStepSucceeded](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s)
          }
          assert((journalEntries(dataDir / "agent-111/state/journal") map { case Stamped(_, keyedEvent) ⇒ keyedEvent }) ==
            Vector(KeyedEvent(AgentEvent.MasterAdded)(UserId.Anonymous)))
          logger.info("\n\n*** RESTARTING AGENTS ***\n")
          runAgents(agentConfs) { _ ⇒
            lastEventId = lastEventIdOf(eventCollector.when[OrderEvent.OrderStepSucceeded](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s)
          }
        }

        for (i ← 1 to 2) {
          val myLastEventId = lastEventId
          sys.runtime.gc()  // For a clean memory view
          logger.info(s"\n\n*** RESTARTING MASTER AND AGENTS #$i ***\n")
          runAgents(agentConfs) { _ ⇒
            runMaster(dataDir) { master ⇒
              val eventSeq = eventCollector.when[OrderEvent.OrderFinished.type](EventRequest.singleClass(after = myLastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s
              val orderId = (eventSeq: @unchecked) match {
                case eventSeq: EventSeq.NonEmpty[Iterator, KeyedEvent[OrderEvent.OrderFinished.type]] ⇒ eventSeq.stampeds.toVector.last.value.key
              }
              master.order(orderId) await 99.s shouldEqual
                Some(Order(
                  orderId,
                  NodeKey(TestJobnetPath, NodeId("END")),
                  Order.Finished,
                  Map("result" → "TEST-RESULT-VALUE-agent-222"),
                  Order.Good(true)))
            }
          }
        }
      }
    }
  }

  private def runMaster(dataDir: Path)(body: Master ⇒ Unit): Unit = {
    withCloser { implicit closer ⇒
      val injector = Guice.createInjector(new MasterModule(MasterConfiguration.forTest(data = Some(dataDir / "master"))))
      eventCollector.start(injector.instance[ActorSystem], injector.instance[StampedKeyedEventBus])
      logger.debug("Close")
      injector.instance[Closer].closeWithCloser
      val master = injector.instance[Master]
      master.start() await 99.s
      master.executeCommand(MasterCommand.ScheduleOrdersEvery(2.s))  // Will block on recovery until Agents are started: await 99.s
      body(master)
    }
  }

  private def runAgents(confs: Seq[AgentConfiguration])(body: Seq[Agent] ⇒ Unit): Unit = {
    multipleAutoClosing(for (o ← confs) yield new Agent(o)) { agents ⇒
      (for (a ← agents) yield a.start()) await 10.s
      body(agents)
      (for (a ← agents) yield a.executeCommand(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(1.s)))) await 10.s
      (for (a ← agents) yield a.terminated) await 10.s
    }
  }

  private def journalEntries(journalFile: Path): Vector[Any] =
    autoClosing(new JsonFileIterator(JsonJournalMeta.Header, in ⇒ new GZIPInputStream(in), journalFile)) {
      _.toVector map AgentActor.MyJournalMeta.deserialize
    }
}

private object RecoveryIT {
  private val AgentNames = List("agent-111", "agent-222")
  private val TestJobnetPath = JobnetPath("/test")
  private val FastJobnetPath = JobnetPath("/fast")

  private val FastOrderId = OrderId("FAST-ORDER")
  private val FastOrder = Order(FastOrderId, NodeKey(FastJobnetPath, NodeId("100")), Order.Waiting)

  private val TestJobChainElem =
    <job_chain>
      <job_chain_node state="100" agent="test-agent-111" job="/test"/>
      <job_chain_node state="110" agent="test-agent-111" job="/test"/>
      <job_chain_node state="130" agent="test-agent-111" job="/test"/>
      <job_chain_node state="200" agent="test-agent-222" job="/test"/>
      <job_chain_node state="210" agent="test-agent-222" job="/test"/>
      <job_chain_node.end state="END"/>
    </job_chain>

  private val TestOrderGeneratorElem =
    <order job_chain={TestJobnetPath.string} state="100">
      <run_time><period absolute_repeat="3"/></run_time>
    </order>

  private val QuickJobChainElem =
    <job_chain>
      <job_chain_node state="100" agent="test-agent-111" job="/test"/>
      <job_chain_node.end state="END"/>
    </job_chain>

  private val logger = Logger(getClass)

  private class DataDirectoryProvider extends HasCloser {
    val dataDir = createTempDirectory("test-") withCloser deleteDirectoryRecursively
    closeOnError(closer) {
      createDirectories(dataDir / "master/config/live")
      for (agentName ← AgentNames) createDirectories(dataDir / s"$agentName/config/live")
    }

    private val testScript =
      if (isWindows) """
        |@echo off
        |ping -n 2 127.0.0.1 >nul
        |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
        |""".stripMargin
      else """
        |sleep 1
        |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
        |""".stripMargin

    for (agentName ← AgentNames) {
      (dataDir / s"$agentName/config/live/test.job.xml").xml =
        <job tasks="3">
          <params>
            <param name="var1" value={s"VALUE-$agentName"}/>
          </params>
          <script language="shell">{testScript}</script>
        </job>
    }
  }

  private def lastEventIdOf[E <: Event](eventSeq: TearableEventSeq[Iterator, KeyedEvent[E]]): EventId =
    (eventSeq: @unchecked) match {
      case eventSeq: EventSeq.NonEmpty[Iterator, KeyedEvent[E]] ⇒
        eventSeq.stampeds.toVector.last.eventId
    }
}
