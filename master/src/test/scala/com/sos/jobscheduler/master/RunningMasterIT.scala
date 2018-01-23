package com.sos.jobscheduler.master

import akka.actor.{Actor, ActorSystem, Props}
import com.google.inject.Injector
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.akkahttp.WebServerBinding
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventRequest, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.{JobPath, Position, WorkflowPath}
import com.sos.jobscheduler.master.RunningMasterIT._
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.order.{MasterOrderKeeper, OrderGeneratorPath}
import com.sos.jobscheduler.master.tests.TestEnvironment
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import java.net.InetSocketAddress
import java.time.Instant
import java.time.Instant.now
import java.util.concurrent.ConcurrentLinkedQueue
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class RunningMasterIT extends FreeSpec {

  "test" in {
    autoClosing(new TestEnvironment(AgentPaths, temporaryDirectory / "RunningMasterIT")) { env ⇒
      val agentConfigs = AgentPaths map { agentPath ⇒
        env.agentXmlFile(agentPath, JobPath("/test")).xml =
          <job>
            <params>
              <param name="var1" value={s"VALUE-${agentPath.withoutStartingSlash}"}/>
            </params>
            <script language="shell">{TestScript}</script>
          </job>
        AgentConfiguration.forTest(Some(env.agentDir(agentPath)))
      }

      val agent0 = RunningAgent.startForTest(agentConfigs(0)) await 10.s
      env.xmlFile(TestWorkflowPath).xml =
        <job_chain>
          <job_chain_node state="100" agent="test-agent-111" job="/test"/>
          <job_chain_node state="200" agent="test-agent-222" job="/test"/>
          <job_chain_node.end state="END"/>
        </job_chain>
      env.xmlFile(OrderGeneratorPath("/test")).xml =
        <order job_chain="test">
          <params>
            <param name="x" value="XXX"/>
          </params>
          <run_time>
            <period absolute_repeat="1"/>
          </run_time>
        </order>
      env.xmlFile(AgentPath("/test-agent-111")).xml =
        <agent uri={agent0.localUri.toString}/>
      val agent1Port = FreeTcpPortFinder.findRandomFreeTcpPort()
      env.xmlFile(AgentPath("/test-agent-222")).xml =
        <agent uri={s"http://127.0.0.1:$agent1Port"}/>

      RunningMaster.run(MasterConfiguration.forTest(configAndData = env.masterDir)) { master ⇒
        import master.{injector, orderClient}
        val lastEventId = injector.instance[EventCollector].lastEventId
        val actorSystem = injector.instance[ActorSystem]
        val eventGatherer = new TestEventGatherer(injector)
        val adHocOrder = Order(TestOrderId, TestWorkflowPath, Order.StartNow)

        sleep(3.s)  // Let OrderGenerator generate some orders
        master.orderKeeper ! MasterOrderKeeper.Input.SuspendDetaching
        val agent1 = RunningAgent.startForTest(agentConfigs(1).copy(http = Some(WebServerBinding.Http(new InetSocketAddress("127.0.0.1", agent1Port))))) await 10.s  // Start early to recover orders
        master.executeCommand(MasterCommand.AddOrderIfNew(adHocOrder)) await 10.s

        val EventSeq.NonEmpty(_) = master.eventCollector.when[OrderEvent.OrderDetachable.type](EventRequest.singleClass(after = lastEventId, 10.s), _.key == TestOrderId) await 99.s
        val agentClients = for (a ← List(agent0, agent1)) yield AgentClient(a.localUri.toString)(actorSystem)
        //sleep(500.ms)  // MasterOrderKeeper allows GetOrders while journaling events (persistAsync). Wait until events are stored and orders are up-to-date. Time-dependend !!!
        assert(agentClients(0).orders() await 99.s map { _.id } contains TestOrderId)
        master.orderKeeper ! MasterOrderKeeper.Input.ContinueDetaching

        master.eventCollector.when[OrderEvent.OrderFinished.type](EventRequest.singleClass(after = lastEventId, 20.s), _.key == TestOrderId) await 99.s
        orderClient.order(TestOrderId) await 10.s shouldEqual
          Some(Order(
            TestOrderId,
            TestWorkflowPath /: Position(2),
            Order.Finished,
            payload = Payload(
              Map("result" → "TEST-RESULT-VALUE-agent-222"),
              Outcome.Good(true))))
        assert(orderClient.orderCount.await(99.s) >= 1)

        master.executeCommand(MasterCommand.ScheduleOrdersEvery((TestDuration / 2).toFiniteDuration)) await 99.s  // Needing 2 consecutive order generations
        val expectedOrderCount = 1 + TestDuration.getSeconds.toInt  // Expecting one finished order per second
        waitForCondition(TestDuration + 10.s, 100.ms) { eventGatherer.orderIdsOf[OrderEvent.OrderFinished.type].size == expectedOrderCount }
        logger.info("Events:\n" + ((eventGatherer.events map { _.toString }) mkString "\n"))
        val orderIds = eventGatherer.orderIdsOf[OrderEvent.OrderFinished.type].toVector
        for (line ← (for (orderId ← orderIds.sorted) yield for (o ← orderClient.order(orderId)) yield s"$orderId -> $o") await 99.s)
          logger.info(line)
        assert(orderIds.size >= expectedOrderCount)
        val addedOrderIds = eventGatherer.orderIdsOf[OrderEvent.OrderAdded] filter orderIds.toSet
        assert(addedOrderIds.size == orderIds.size)
        assert(eventGatherer.orderIdsOf[OrderEvent.OrderFinished.type] == addedOrderIds)

        master.executeCommand(MasterCommand.Terminate) await 99.s
        master.terminated await 99.s
        master.close()
        agent1.terminate() await 99.s
        agent1.close()
      }
      agent0.terminate() await 99.s
      agent0.close()
    }
  }
}

private object RunningMasterIT {
  private val TestDuration = 10.s
  private val TestWorkflowPath = WorkflowPath("/test")
  private val TestOrderId = OrderId("ORDER-ID")
  private val AgentPaths = List(AgentPath("/agent-111"), AgentPath("/agent-222"))
  private val logger = Logger(getClass)

  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private class TestEventGatherer(injector: Injector) {
    import TestEventGatherer._
    private val _events = new ConcurrentLinkedQueue[Entry]

    def events = _events.asScala

    def orderIdsOf[E <: OrderEvent: ClassTag]: Set[OrderId] = (collect[E] map { _.key }).toSet

    def collect[E <: OrderEvent: ClassTag] = _events.asScala collect {
      case Entry(_, KeyedEvent(orderId: OrderId, e: OrderEvent)) if implicitClass[E] isAssignableFrom e.getClass ⇒
        KeyedEvent[OrderEvent](orderId, e)
    }

    injector.instance[ActorSystem] actorOf Props {
      new Actor {
        injector.instance[StampedKeyedEventBus].subscribe(self, classOf[Event])
        def receive = {
          case Stamped(_, e: AnyKeyedEvent) ⇒ _events.add(Entry(now, e))
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
