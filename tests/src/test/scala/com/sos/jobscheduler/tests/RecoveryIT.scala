package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.google.inject.Guice
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.scheduler.{AgentActor, AgentEvent}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, closeOnError, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.jobnet.{JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.Order.Scheduled
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMovedToAgent, OrderMovedToMaster, OrderStdoutWritten, OrderStepFailed, OrderStepStarted, OrderStepSucceeded}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.configuration.inject.MasterModule
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{JsonFileIterator, JsonJournalMeta}
import com.sos.jobscheduler.tests.RecoveryIT._
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory}
import java.nio.file.Path
import java.time.Instant
import java.util.zip.GZIPInputStream
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json.JsObject

/**
  * @author Joacim Zschimmer
  */
final class RecoveryIT extends FreeSpec {

  private val eventCollector = new TestEventCollector

  "test" in {
    var lastEventId = EventId.BeforeFirst
    autoClosing(new DirectoryProvider) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        import directoryProvider.directory

        val agentConfs = for (name ← AgentNames) yield AgentConfiguration.forTest(Some(directory / name)).copy(name = name)

        (directory / "master/config/live/fast.job_chain.xml").xml = QuickJobChainElem
        (directory / "master/config/live/test.job_chain.xml").xml = TestJobChainElem
        (directory / "master/config/live/test.order.xml").xml = TestOrderGeneratorElem
        (directory / "master/config/live/test-agent-111.agent.xml").xml = <agent uri={agentConfs(0).localUri.toString}/>
        (directory / "master/config/live/test-agent-222.agent.xml").xml = <agent uri={agentConfs(1).localUri.toString}/>

        runMaster(directory) { master ⇒
          if (lastEventId == EventId.BeforeFirst) {
            lastEventId = eventCollector.oldestEventId
          }
          runAgents(agentConfs) { _ ⇒
            master.executeCommand(MasterCommand.AddOrderIfNew(FastOrder)) await 99.s
            lastEventId = lastEventIdOf(eventCollector.when[OrderFinished.type](EventRequest.singleClass(after = lastEventId, 99.s), _.key == FastOrderId) await 99.s)
            lastEventId = lastEventIdOf(eventCollector.when[OrderStepSucceeded](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s)
            lastEventId = lastEventIdOf(eventCollector.when[OrderStepSucceeded](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s)
          }
          assert((readEvents(directory / "agent-111/data/state/journal") map { case Stamped(_, keyedEvent) ⇒ keyedEvent }) ==
            Vector(KeyedEvent(AgentEvent.MasterAdded)(UserId.Anonymous)))
          logger.info("\n\n*** RESTARTING AGENTS ***\n")
          runAgents(agentConfs) { _ ⇒
            lastEventId = lastEventIdOf(eventCollector.when[OrderStepSucceeded](EventRequest.singleClass(after = lastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s)
          }
        }

        for (i ← 1 to 2) withClue(s"Run #$i") {
          val myLastEventId = lastEventId
          sys.runtime.gc()  // For a clean memory view
          logger.info(s"\n\n*** RESTARTING MASTER AND AGENTS #$i ***\n")
          runAgents(agentConfs) { _ ⇒
            runMaster(directory) { master ⇒
              val finishedEventSeq = eventCollector.when[OrderFinished.type](EventRequest.singleClass(after = myLastEventId, 99.s), _.key.string startsWith TestJobnetPath.string) await 99.s
              val orderId = (finishedEventSeq: @unchecked) match {
                case eventSeq: EventSeq.NonEmpty[Iterator, KeyedEvent[OrderFinished.type]] ⇒ eventSeq.stampeds.toVector.last.value.key
              }
              master.orderClient.order(orderId) await 99.s shouldEqual
                Some(Order(
                  orderId,
                  NodeKey(TestJobnetPath, NodeId("END")),
                  Order.Finished,
                  Map("result" → "TEST-RESULT-VALUE-agent-222"),
                  Order.Good(true)))
              val EventSeq.NonEmpty(eventSeq) = eventCollector.when[OrderEvent](EventRequest.singleClass(after = EventId.BeforeFirst, 0.s), _.key == orderId) await 99.s
              withClue(s"$orderId") {
                assertResult(ExpectedEvents) {
                  (eventSeq map { _.value.event } collect {
                    case o @ OrderAdded(_, Order.Scheduled(_), _, _) ⇒ o.copy(state = Order.Scheduled(SomeInstant))
                    case o if !isIgnoredEvent(o) ⇒ o
                  }).toVector
                }
              }
            }
          }
        }
      }
    }
  }

  private def runMaster(directory: Path)(body: RunningMaster ⇒ Unit): Unit = {
    val injector = Guice.createInjector(new MasterModule(MasterConfiguration.forTest(configAndData = directory / "master")))
    eventCollector.start(injector.instance[ActorSystem], injector.instance[StampedKeyedEventBus])
    val master = RunningMaster(injector) await 99.s
    master.executeCommand(MasterCommand.ScheduleOrdersEvery(2.s))  // Will block on recovery until Agents are started: await 99.s
    body(master)
    master.executeCommand(MasterCommand.Terminate) await 99.s
    master.terminated await 99.s
  }

  private def runAgents(confs: Seq[AgentConfiguration])(body: Seq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(confs map { o ⇒ RunningAgent(o) } await 10.s)(body)

  private def readEvents(journalFile: Path): Vector[Stamped[KeyedEvent[AgentEvent]]] = {
    import AgentActor.MyJournalMeta.eventJsonFormat
    autoClosing(new JsonFileIterator(JsonJournalMeta.Header, in ⇒ new GZIPInputStream(in), journalFile)) {
      _.toVector collect {
        case o: JsObject if eventJsonFormat.canDeserialize(o) ⇒
          o.convertTo[Stamped[KeyedEvent[AgentEvent]]]
      }
    }
  }
}

private object RecoveryIT {
  private val AgentNames = List("agent-111", "agent-222")
  private val TestJobnetPath = JobnetPath("/test")
  private val FastJobnetPath = JobnetPath("/fast")

  private val FastOrderId = OrderId("FAST-ORDER")
  private val FastOrder = Order(FastOrderId, NodeKey(FastJobnetPath, NodeId("100")), Order.Ready)
  private val SomeInstant = Instant.parse("2017-07-23T12:00:00Z")

  private val TestJobChainElem =
    <job_chain>
      <job_chain_node state="100" agent="test-agent-111" job="/test"/>
      <job_chain_node state="110" agent="test-agent-111" job="/test"/>
      <job_chain_node state="120" agent="test-agent-111" job="/test"/>
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

  private val ExpectedEvents = Vector(
    OrderAdded(NodeKey(TestJobnetPath, NodeId("100")), Scheduled(SomeInstant),Map(),Order.Good(true)),
    OrderMovedToAgent(AgentPath("/test-agent-111")),
    //OrderStepStarted,
    //OrderStdoutWritten("TEST\n"),
    OrderStepSucceeded(MapDiff(Map("result" → "TEST-RESULT-VALUE-agent-111"), Set()), true, NodeId("110")),
    //OrderStepStarted,
    //OrderStdoutWritten("TEST\n"),
    OrderStepSucceeded(MapDiff(Map(), Set()), true, NodeId("120")),
    //OrderStepStarted,
    //OrderStdoutWritten("TEST\n"),
    OrderStepSucceeded(MapDiff(Map(), Set()), true, NodeId("200")),
    OrderDetachable,
    OrderMovedToMaster,
    OrderMovedToAgent(AgentPath("/test-agent-222")),
    //OrderStepStarted,
    //OrderStdoutWritten("TEST\n"),
    OrderStepSucceeded(MapDiff(Map("result" → "TEST-RESULT-VALUE-agent-222"), Set()), true, NodeId("210")),
    //OrderStepStarted,
    //OrderStdoutWritten("TEST\n"),
    OrderStepSucceeded(MapDiff(Map(), Set()), true, NodeId("END")),
    OrderDetachable,
    OrderMovedToMaster,
    OrderFinished)
  private def isIgnoredEvent(event: OrderEvent) =
    event.isInstanceOf[OrderStepFailed] || // May occur or not in this test
    event == OrderStepStarted ||  // May occur duplicate after job restart
    event.isInstanceOf[OrderStdoutWritten]  // May occur duplicate after job restart


  private val logger = Logger(getClass)

  private class DirectoryProvider extends HasCloser {
    val directory = createTempDirectory("test-") withCloser deleteDirectoryRecursively
    closeOnError(closer) {
      createDirectories(directory / "master/config/live")
      createDirectory(directory / "master/data")
      for (agentName ← AgentNames) {
        createDirectories(directory / s"$agentName/config/live")
        createDirectory(directory / s"$agentName/data")
      }
    }

    private val testScript =
      if (isWindows) """
        |@echo off
        |echo TEST
        |ping -n 2 127.0.0.1 >nul
        |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
        |""".stripMargin
      else """
        |echo TEST
        |sleep 1
        |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
        |""".stripMargin

    for (agentName ← AgentNames) {
      (directory / s"$agentName/config/live/test.job.xml").xml =
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
