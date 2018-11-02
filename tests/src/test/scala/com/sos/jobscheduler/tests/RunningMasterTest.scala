package com.sos.jobscheduler.tests

import akka.actor.{Actor, ActorSystem, Props}
import com.google.inject.Injector
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension ⇒ sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.tests.RunningMasterTest._
import java.time.Instant
import java.time.Instant.now
import java.util.concurrent.ConcurrentLinkedQueue
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class RunningMasterTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(AgentPaths)) { provider ⇒
      for (agentPath ← AgentPaths) {
        provider.agentToTree(agentPath).writeExecutable(ExecutablePath(s"/TEST$sh"), TestScript)
      }

      val agent0 = provider.startAgent(AgentPaths(0)) await 10.s
      provider.master.writeJson(
        Workflow(TestWorkflowPath % VersionId.Anonymous,
          Vector(
            Execute(WorkflowJob(AgentPaths(0), ExecutablePath(s"/TEST$sh"))),
            Execute(WorkflowJob(AgentPaths(1), ExecutablePath(s"/TEST$sh"))))))
      (provider.master.orderGenerators / "test.order.xml").xml =
        <order job_chain="test">
          <params>
            <param name="x" value="XXX"/>
          </params>
          <run_time>
            <period absolute_repeat="1"/>
          </run_time>
        </order>

      RunningMaster.run(MasterConfiguration.forTest(configAndData = provider.master.directory)) { master ⇒
        import master.{injector, orderApi}

        assert((master.injector.instance[MasterConfiguration].stateDirectory / "http-uri").contentString == s"${master.localUri}/master")

        val eventGatherer = new TestEventGatherer(injector)

        sleep(3.s)  // Let OrderGenerator generate some orders
        val agent1 = provider.startAgent(AgentPaths(1)) await 10.s  // Start early to recover orders
        master.addOrderBlocking(FreshOrder(TestOrderId, TestWorkflowId.path))

        master.eventWatch.await[OrderEvent.OrderFinished](_.key == TestOrderId, timeout = 20.seconds)
        //waitForCondition(10.s, 10.ms) { orderApi.orderCount.await(99.s) == 0 }   // MasterOrderKeeper updates orderCount with persitAsync
        assert(orderApi.orderCount.await(99.s) == 0)

        master.executeCommandAsSystemUser(MasterCommand.ScheduleOrdersEvery((TestDuration / 2).toFiniteDuration)) await 99.s  // Needing 2 consecutive order generations
        val expectedOrderCount = 1 + TestDuration.getSeconds.toInt  // Expecting one finished order per second
        waitForCondition(TestDuration + 10.s, 100.ms) { eventGatherer.orderIdsOf[OrderEvent.OrderFinished].size == expectedOrderCount }
        logger.info("Events:\n" + ((eventGatherer.events map { _.toString }) mkString "\n"))
        val orderIds = eventGatherer.orderIdsOf[OrderEvent.OrderFinished].toVector
        for (line ← (for (orderId ← orderIds.sorted) yield
                       for (o ← orderApi.order(orderId).runAsync: Future[Option[Order[Order.State]]])
                          yield s"$orderId -> $o") await 99.s)
          logger.info(line)
        assert(orderIds.size >= expectedOrderCount)
        val addedOrderIds = eventGatherer.orderIdsOf[OrderEvent.OrderAdded] filter orderIds.toSet
        assert(addedOrderIds.size == orderIds.size)
        assert(eventGatherer.orderIdsOf[OrderEvent.OrderFinished] == addedOrderIds)

        master.terminate() await 99.s
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
