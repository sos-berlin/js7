package com.sos.jobscheduler.master

import akka.actor.{Actor, ActorSystem, Props}
import com.google.inject.Injector
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.problem.Checked.Ops
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
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.master.RunningMasterTest._
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.order.ScheduledOrderGeneratorPath
import com.sos.jobscheduler.master.tests.TestEnvironment
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
final class RunningMasterTest extends FreeSpec {

  "test" in {
    autoClosing(new TestEnvironment(AgentPaths, temporaryDirectory / "RunningMasterTest")) { env ⇒
      val agentConfigs = AgentPaths map { agentPath ⇒
        env.agentFile(agentPath, JobPath("/test"), SourceType.Xml).xml =
          <job>
            <params>
              <param name="var1" value={s"VALUE-${agentPath.withoutStartingSlash}"}/>
            </params>
            <script language="shell">{TestScript}</script>
          </job>
        AgentConfiguration.forTest(Some(env.agentDir(agentPath)))
      }

      val agent0 = RunningAgent.startForTest(agentConfigs(0)) await 10.s
      env.file(TestWorkflowPath, SourceType.Xml).xml =
        <job_chain>
          <job_chain_node state="100" agent="test-agent-111" job="/test"/>
          <job_chain_node state="200" agent="test-agent-222" job="/test"/>
          <job_chain_node.end state="END"/>
        </job_chain>
      env.file(ScheduledOrderGeneratorPath("/test"), SourceType.Xml).xml =
        <order job_chain="test">
          <params>
            <param name="x" value="XXX"/>
          </params>
          <run_time>
            <period absolute_repeat="1"/>
          </run_time>
        </order>
      env.file(AgentPath("/test-agent-111"), SourceType.Xml).xml = <agent uri={agent0.localUri.toString}/>
      val agent1Port = FreeTcpPortFinder.findRandomFreeTcpPort()
      env.file(AgentPath("/test-agent-222"), SourceType.Xml).xml = <agent uri={s"http://127.0.0.1:$agent1Port"}/>

      RunningMaster.run(MasterConfiguration.forTest(configAndData = env.masterDir)) { master ⇒
        import master.{injector, orderClient}
        val lastEventId = injector.instance[EventCollector].lastEventId
        val eventGatherer = new TestEventGatherer(injector)

        sleep(3.s)  // Let OrderGenerator generate some orders
        val agent1 = RunningAgent.startForTest(agentConfigs(1).copy(http = Some(WebServerBinding.Http(new InetSocketAddress("127.0.0.1", agent1Port))))) await 10.s  // Start early to recover orders
        master.addOrder(FreshOrder(TestOrderId, TestWorkflowId.path)).await(10.s).orThrow

        master.eventCollector.when[OrderEvent.OrderFinished](EventRequest.singleClass(after = lastEventId, 20.s), _.key == TestOrderId) await 99.s
        //Order has been deleted after OrderFinished:
        //orderClient.order(TestOrderId) await 10.s shouldEqual
        //  Some(Order(
        //    TestOrderId,
        //    TestWorkflowId /: Position(2),
        //    Order.Finished,
        //    payload = Payload(Map("result" → "TEST-RESULT-VALUE-agent-222"))))
        assert(orderClient.orderCount.await(99.s) == 0)

        master.executeCommand(MasterCommand.ScheduleOrdersEvery((TestDuration / 2).toFiniteDuration)) await 99.s  // Needing 2 consecutive order generations
        val expectedOrderCount = 1 + TestDuration.getSeconds.toInt  // Expecting one finished order per second
        waitForCondition(TestDuration + 10.s, 100.ms) { eventGatherer.orderIdsOf[OrderEvent.OrderFinished].size == expectedOrderCount }
        logger.info("Events:\n" + ((eventGatherer.events map { _.toString }) mkString "\n"))
        val orderIds = eventGatherer.orderIdsOf[OrderEvent.OrderFinished].toVector
        for (line ← (for (orderId ← orderIds.sorted) yield for (o ← orderClient.order(orderId)) yield s"$orderId -> $o") await 99.s)
          logger.info(line)
        assert(orderIds.size >= expectedOrderCount)
        val addedOrderIds = eventGatherer.orderIdsOf[OrderEvent.OrderAdded] filter orderIds.toSet
        assert(addedOrderIds.size == orderIds.size)
        assert(eventGatherer.orderIdsOf[OrderEvent.OrderFinished] == addedOrderIds)

        master.executeCommand(MasterCommand.Terminate) await 99.s
        master.terminated await 99.s
        agent1.terminate() await 99.s
        agent1.close()
      }
      agent0.terminate() await 99.s
      agent0.close()
    }
  }
}

private object RunningMasterTest {
  private val TestDuration = 10.s
  private val TestWorkflowPath = WorkflowPath("/test")
  private val TestWorkflowId = TestWorkflowPath % "(initial)"
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
          case Stamped(_, _, e: AnyKeyedEvent) ⇒ _events.add(Entry(now, e))
        }
      }
    }
  }

  private object TestEventGatherer {
    final case class Entry(instant: Instant, keyedEvent: AnyKeyedEvent) {
      override def toString = s"$instant $keyedEvent"
    }
  }
}
