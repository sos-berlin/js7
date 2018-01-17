package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, multipleAutoClosing}
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.LineEnd
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderMovedToAgent, OrderMovedToMaster, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.InstructionNr
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobXml}
import com.sos.jobscheduler.tests.ForkTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds

final class ForkTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(AAgentPath, BAgentPath))) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        import directoryProvider.directory

        directoryProvider.master.jsonFile(TestNamedWorkflow.path).contentString = TestWorkflow.asJson.toPrettyString
        for (a ← directoryProvider.agents) a.job(TestJobPath).xml = jobXml(100.ms)

        runAgents(directoryProvider.agents map (_.conf)) { _ ⇒
          RunningMaster.runForTest(directory) { master ⇒
            val eventCollector = new TestEventCollector
            eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
            master.executeCommand(MasterCommand.AddOrderIfNew(TestOrder)) await 99.s
            val EventSeq.NonEmpty(_) = eventCollector.when[OrderFinished.type](
              EventRequest.singleClass(after = EventId.BeforeFirst, 99.s), _.key.string startsWith TestOrder.id.string) await 99.s
            val eventSeq = eventCollector.byPredicate[OrderEvent](EventRequest.singleClass(after = EventId.BeforeFirst, timeout = 0.s), _ ⇒ true) await 99.s
            checkEventSeq(eventSeq)
          }
        }
      }
    }
  }

  private def runAgents(confs: Seq[AgentConfiguration])(body: Seq[RunningAgent] ⇒ Unit): Unit =
    multipleAutoClosing(confs map startAgent await 10.s) { agents ⇒
      body(agents)
    }

  private def startAgent(conf: AgentConfiguration): Future[RunningAgent] = {
    val whenAgent = RunningAgent(conf)
    for (agent ← whenAgent; t ← agent.terminated.failed) logger.error(t.toStringWithCauses, t)
    whenAgent
  }

  private def checkEventSeq(eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val keyedEvents = stampeds.map(_.value).toVector
        for (orderId ← Array(TestOrder.id, XOrderId, YOrderId)) {  // But ordering if each order is determined
          assert(keyedEvents.filter(_.key == orderId) == ExpectedEvents.filter(_.key == orderId))
        }
        assert(keyedEvents.toSet == ExpectedEvents.toSet)  // XOrderId and YOrderId run in parallel and ordering is not determined
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object ForkTest {
  def nr(n: Int) = InstructionNr(n)
  private val TestOrder = Order(OrderId("🔺"), TestNamedWorkflow.path, state = Order.StartNow, payload = Payload(Map("VARIABLE" → "VALUE")))
  private val XOrderId = OrderId(s"🔺/🥕")
  private val YOrderId = OrderId(s"🔺/🍋")
  val ExpectedEvents = Vector(
    KeyedEvent(OrderAdded(TestNamedWorkflow.path, Order.StartNow, Payload(Map("VARIABLE" → "VALUE"))))(TestOrder.id),
    KeyedEvent(OrderMovedToAgent(AAgentPath))                     (TestOrder.id),
    KeyedEvent(OrderProcessingStarted)                            (TestOrder.id),
    KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (TestOrder.id),
    KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (TestOrder.id),
    KeyedEvent(OrderMoved(1))                                     (TestOrder.id),

    KeyedEvent(OrderForked(Vector(
      OrderForked.Child(OrderId.ChildId("🥕"), XOrderId, MapDiff.empty),                  OrderForked.Child(OrderId.ChildId("🍋"), YOrderId, MapDiff.empty)))) (TestOrder.id),
    KeyedEvent(OrderDetachable)                                   (TestOrder.id),
    KeyedEvent(OrderMovedToMaster)                                (TestOrder.id),
      KeyedEvent(OrderProcessingStarted)                                  (XOrderId),   KeyedEvent(OrderProcessingStarted)                            (YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))            (XOrderId),   KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))       (XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (YOrderId),
      KeyedEvent(OrderMoved(1))                                           (XOrderId),   KeyedEvent(OrderMoved(1))                                     (YOrderId),
                                                                                        KeyedEvent(OrderDetachable)                                   (YOrderId),
                                                                                        KeyedEvent(OrderMovedToMaster)                                (YOrderId),
                                                                                        KeyedEvent(OrderMovedToAgent(BAgentPath))                     (YOrderId),
      KeyedEvent(OrderProcessingStarted)                                  (XOrderId),   KeyedEvent(OrderProcessingStarted)                            (YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))            (XOrderId),   KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))       (XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (YOrderId),
      KeyedEvent(OrderMoved(2))                                           (XOrderId),   KeyedEvent(OrderMoved(2))                                     (YOrderId),
      KeyedEvent(OrderDetachable)                                         (XOrderId),   KeyedEvent(OrderDetachable)                                   (YOrderId),
    KeyedEvent(OrderMovedToMaster)                                      (XOrderId),   KeyedEvent(OrderMovedToMaster)                                (YOrderId),
    KeyedEvent(OrderJoined(2, MapDiff.empty, Outcome.Good(true))) (TestOrder.id),

    KeyedEvent(OrderMovedToAgent(AAgentPath))                     (TestOrder.id),
    KeyedEvent(OrderProcessingStarted)                            (TestOrder.id),
    KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (TestOrder.id),
    KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (TestOrder.id),
    KeyedEvent(OrderMoved(3))                                     (TestOrder.id),
    KeyedEvent(OrderForked(Vector(
      OrderForked.Child(OrderId.ChildId("🥕"), XOrderId, MapDiff.empty),                  OrderForked.Child(OrderId.ChildId("🍋"), YOrderId, MapDiff.empty)))) (TestOrder.id),
    KeyedEvent(OrderDetachable)                                   (TestOrder.id),
    KeyedEvent(OrderMovedToMaster)                                (TestOrder.id),
      KeyedEvent(OrderProcessingStarted)                                  (XOrderId),   KeyedEvent(OrderProcessingStarted)                            (YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))            (XOrderId),   KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))       (XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (YOrderId),
      KeyedEvent(OrderMoved(1))                                           (XOrderId),   KeyedEvent(OrderMoved(1))                                     (YOrderId),
      KeyedEvent(OrderProcessingStarted)                                  (XOrderId),   KeyedEvent(OrderProcessingStarted)                            (YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))            (XOrderId),   KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))       (XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (YOrderId),
      KeyedEvent(OrderMoved(2))                                           (XOrderId),   KeyedEvent(OrderMoved(2))                                     (YOrderId),
      KeyedEvent(OrderDetachable)                                         (XOrderId),   KeyedEvent(OrderDetachable)                                   (YOrderId),
      KeyedEvent(OrderMovedToMaster)                                      (XOrderId),   KeyedEvent(OrderMovedToMaster)                                (YOrderId),
    KeyedEvent(OrderJoined(4, MapDiff.empty, Outcome.Good(true))) (TestOrder.id),

    KeyedEvent(OrderMovedToAgent(AAgentPath))                     (TestOrder.id),
    KeyedEvent(OrderProcessingStarted)                            (TestOrder.id),
    KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (TestOrder.id),
    KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (TestOrder.id),
    KeyedEvent(OrderMoved(5))                                     (TestOrder.id),
  //KeyedEvent(OrderDetachable)                                   (TestOrder.id),
  //KeyedEvent(OrderMovedToMaster)                                (TestOrder.id),

    KeyedEvent(OrderForked(Vector(
      OrderForked.Child(OrderId.ChildId("🥕"), XOrderId, MapDiff.empty),                  OrderForked.Child(OrderId.ChildId("🍋"), YOrderId, MapDiff.empty)))) (TestOrder.id),
    KeyedEvent(OrderDetachable)                                   (TestOrder.id),
    KeyedEvent(OrderMovedToMaster)                                (TestOrder.id),
                                                                                        KeyedEvent(OrderDetachable)                                   (YOrderId),
                                                                                        KeyedEvent(OrderMovedToMaster)                                (YOrderId),
                                                                                        KeyedEvent(OrderMovedToAgent(BAgentPath))                     (YOrderId),
      KeyedEvent(OrderProcessingStarted)                                  (XOrderId),   KeyedEvent(OrderProcessingStarted)                            (YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))            (XOrderId),   KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))       (XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (YOrderId),
      KeyedEvent(OrderMoved(1))                                           (XOrderId),   KeyedEvent(OrderMoved(1))                                     (YOrderId),
      KeyedEvent(OrderProcessingStarted)                                  (XOrderId),   KeyedEvent(OrderProcessingStarted)                            (YOrderId),
      KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))            (XOrderId),   KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (YOrderId),
      KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true)))       (XOrderId),   KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (YOrderId),
      KeyedEvent(OrderMoved(2))                                           (XOrderId),   KeyedEvent(OrderMoved(2))                                     (YOrderId),
      KeyedEvent(OrderDetachable)                                         (XOrderId),   KeyedEvent(OrderDetachable)                                   (YOrderId),
      KeyedEvent(OrderMovedToMaster)                                      (XOrderId),   KeyedEvent(OrderMovedToMaster)                                (YOrderId),
    KeyedEvent(OrderJoined(6, MapDiff.empty, Outcome.Good(true))) (TestOrder.id),

    KeyedEvent(OrderMovedToAgent(AAgentPath))                     (TestOrder.id),
    KeyedEvent(OrderProcessingStarted)                            (TestOrder.id),
    KeyedEvent(OrderStdoutWritten(s"$StdoutOutput$LineEnd"))      (TestOrder.id),
    KeyedEvent(OrderProcessed(MapDiff.empty, Outcome.Good(true))) (TestOrder.id),
    KeyedEvent(OrderMoved(7))                                     (TestOrder.id),
    KeyedEvent(OrderDetachable)                                   (TestOrder.id),
    KeyedEvent(OrderMovedToMaster)                                (TestOrder.id),
    KeyedEvent(OrderFinished)                                     (TestOrder.id))

  private val logger = Logger(getClass)
}
