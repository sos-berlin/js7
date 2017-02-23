package com.sos.scheduler.engine.master

import akka.actor.{Actor, ActorSystem, Props}
import com.google.common.io.Closer
import com.google.inject.{Guice, Injector}
import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.client.AgentClient
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.common.event.collector.EventCollector
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.common.scalautil.AutoClosing.{autoClosing, closeOnError}
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.{RichClosersAny, RichClosersAutoCloseable}
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.data.engine2.order.{JobChainPath, NodeId, NodeKey, Order, OrderEvent}
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, KeyedEvent, Snapshot}
import com.sos.scheduler.engine.data.order.OrderId
import com.sos.scheduler.engine.master.MasterIT._
import com.sos.scheduler.engine.master.command.MasterCommand
import com.sos.scheduler.engine.master.configuration.MasterConfiguration
import com.sos.scheduler.engine.master.configuration.inject.MasterModule
import com.sos.scheduler.engine.master.oldruntime.InstantInterval
import com.sos.scheduler.engine.master.order.MasterOrderKeeper
import com.sos.scheduler.engine.shared.event.SnapshotKeyedEventBus
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.time.Instant
import java.time.Instant.now
import java.util.concurrent.ConcurrentLinkedQueue
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class MasterIT extends FreeSpec {

  "test" in {
    autoClosing(new DataDirectoryProvider) { dataDirectoryProvider ⇒
      withCloser { implicit closer ⇒
        import dataDirectoryProvider.dataDir

        val agents = AgentNames map { agentName ⇒
          val agentConf = AgentConfiguration.forTest().copy(
            dataDirectory = Some(dataDir / agentName),
            experimentalOrdersEnabled = true)
          new Agent(agentConf).closeWithCloser
        }

        agents(0).start() await 10.s
        (dataDir / "master/config/live/test.job_chain.xml").xml =
          <job_chain>
            <job_chain_node state="100" agent="test-agent-111" job="/test"/>
            <job_chain_node state="200" agent="test-agent-222" job="/test"/>
            <job_chain_node.end state="END"/>
          </job_chain>
        (dataDir / "master/config/live/test.order.xml").xml =
          <order job_chain="test" state="100">
            <params>
              <param name="x" value="XXX"/>
            </params>
            <run_time absolute_repeat="1"/>
          </order>
        (dataDir / "master/config/live/test-agent-111.agent.xml").xml =
          <agent uri={agents(0).localUri.string}/>
        (dataDir / "master/config/live/test-agent-222.agent.xml").xml =
          <agent uri={agents(1).localUri.string}/>

        val injector = Guice.createInjector(new MasterModule(MasterConfiguration.forTest(data = Some(dataDir / "master"))))
        injector.instance[Closer].closeWithCloser
        val lastEventId = injector.instance[EventCollector].lastEventId
        val actorSystem = injector.instance[ActorSystem]
        val agentClients = for (a ← agents) yield AgentClient(a.localUri.string)(actorSystem)
        val eventGatherer = new TestEventGatherer(injector)
        val master = injector.instance[Master]
        master.start() await 99.s
        val order = Order(
          TestOrderId,
          NodeKey(TestJobChainPath, NodeId("100")),
          Order.Waiting)

        sleep(3.s)
        master.orderKeeper ! MasterOrderKeeper.Input.SuspendDetaching
        agents(1).start() await 10.s  // Start early to recover orders
        master.executeCommand(MasterCommand.AddOrderIfNew(order)) await 10.s

        // Without order recovery: sleep(5.s)
        master.eventCollector.when[OrderEvent.OrderReady.type](EventRequest.singleClass(after = lastEventId, 10.s), _.key == TestOrderId) await 99.s
        assert(agentClients(0).orders() await 99.s map { _.id } contains TestOrderId)
        master.orderKeeper ! MasterOrderKeeper.Input.ContinueDetaching
        // Without order recovery: agents(1).start() await 10.s  // Start late to test late Agent connection

        master.eventCollector.when[OrderEvent.OrderFinished.type](EventRequest.singleClass(after = lastEventId, 20.s), _.key == TestOrderId) await 99.s
        master.getOrder(TestOrderId) await 10.s shouldEqual
          Some(Order(
            TestOrderId,
            NodeKey(TestJobChainPath, NodeId("END")),
            Order.Finished,
            Map("result" → "TEST-RESULT-VALUE-agent-222"),
            Order.Good(true)))

        val scheduledOrders = master.addScheduledOrders(InstantInterval(now, TestDuration))
        val orderIds = order.id +: (scheduledOrders map { _.id })
        val orderCount = orderIds.size
        sleep(TestDuration)  // Time to show DeadLetter ?
        logger.info("Events:\n" + ((eventGatherer.events map { _.toString }) mkString "\n"))
        waitForCondition(TestDuration + 10.s, 100.ms) { eventGatherer.orderIdsOf[OrderEvent.OrderFinished.type].size == orderCount }
        for (line ← (for (orderId ← orderIds.sorted) yield for (o ← master.getOrder(orderId)) yield s"$orderId -> $o") await 99.s)
          logger.info(line)
        assert(eventGatherer.orderIdsOf[OrderEvent.OrderFinished.type].size == orderCount)
        val addedOrderIds = eventGatherer.orderIdsOf[OrderEvent.OrderAdded]
        assert(addedOrderIds.size == orderCount)
        assert(eventGatherer.orderIdsOf[OrderEvent.OrderFinished.type] == addedOrderIds)
      }
    }
  }
}

private object MasterIT {
  private val TestDuration = 10.s
  private val TestJobChainPath = JobChainPath("/test")
  private val TestOrderId = OrderId("ORDER-ID")
  private val AgentNames = List("agent-111", "agent-222")
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
        |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
        |""".stripMargin
      else """
        |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
        |""".stripMargin

    for (agentName ← AgentNames) {
      (dataDir / s"$agentName/config/live/test.job.xml").xml =
        <job>
          <params>
            <param name="var1" value={s"VALUE-$agentName"}/>
          </params>
          <script language="shell">{testScript}</script>
        </job>
    }
  }

  private class TestEventGatherer(injector: Injector) {
    import TestEventGatherer._
    val events = new ConcurrentLinkedQueue[Entry]

    def orderIdsOf[E <: OrderEvent: ClassTag]: Set[OrderId] = (collect[E] map { _.key }).toSet

    def collect[E <: OrderEvent: ClassTag] = events collect {
      case Entry(_, KeyedEvent(orderId: OrderId, e: OrderEvent)) if implicitClass[E] isAssignableFrom e.getClass ⇒
        KeyedEvent[OrderEvent](orderId, e)
    }

    injector.instance[ActorSystem] actorOf Props {
      new Actor {
        injector.instance[SnapshotKeyedEventBus].subscribe(self, classOf[Event])
        def receive = {
          case Snapshot(_, e: AnyKeyedEvent) ⇒ events.add(Entry(now, e))
        }
      }
    }
  }

  private object TestEventGatherer {
    final case class Entry(instant: Instant, keyedEvent: AnyKeyedEvent) {
      override def toString = s"$instant ${keyedEvent.key} ${keyedEvent.event}"
    }
  }
}
