package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{Actor, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, Akkas}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonFormat
import com.sos.jobscheduler.agent.scheduler.job.task.{SimpleShellTaskRunner, TaskRunner}
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobRunner, JobScript}
import com.sos.jobscheduler.agent.scheduler.order.OrderActorTest._
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider.provideAgent2Directory
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedTypedEventJsonFormat.KeyedSubtype
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderDetached, OrderStdWritten, OrderStepEnded, OrderStepStarted, OrderStepSucceeded}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.system.StdoutStderr.{Stderr, Stdout, StdoutStderrType}
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{JsonJournalActor, JsonJournalMeta}
import com.sos.jobscheduler.taskserver.modules.shell.StandardRichProcessStartSynchronizer
import java.nio.file.Path
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class OrderActorTest extends FreeSpec {

  "TEST" in {
    provideAgent2Directory { dir ⇒
      withCloser { implicit closer ⇒
        val actorSystem = Akkas.newActorSystem("OrderActorTest")
        val terminatedPromise = Promise[Completed]()
        actorSystem.actorOf(Props { new TestActor(dir, terminatedPromise) }, "OrderActorTest")
        terminatedPromise.future await 99.s
      }
    }
  }
}

private object OrderActorTest {
  private val TestNodeId = NodeId("NODE-ID")
  private val TestOrder = Order(OrderId("TEST-ORDER"), NodeKey(JobnetPath("/JOBNET"), TestNodeId), Order.Waiting)
  private val TestJobPath = JobPath("/test")
  private val TestJobNode = Jobnet.JobNode(TestNodeId, AgentPath("/TEST-AGENT"), TestJobPath, onSuccess = NodeId("SUCCESS"), onFailure = NodeId("FAILURE"))
  private val ExpectedOrderEvents = List(
    OrderAttached(TestOrder.nodeKey, Order.Waiting, Map(), Order.Good(true)),
    OrderStepStarted,
    OrderStepSucceeded(MapDiff(Map("result" → "TEST-RESULT-FROM-JOB")), returnValue = true, TestJobNode.onSuccess),
    OrderDetached)

  private val TestJobConfiguration = JobConfiguration(TestJobPath,
    JobScript(
      if (isWindows) """
        |@echo off
        |echo Hej!
        |echo THIS IS STDERR>&2
        |echo var1=%SCHEDULER_PARAM_VAR1%
        |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
        |""".stripMargin
      else """
        |echo "Hej!"
        |echo THIS IS STDERR >&2
        |echo "var1=$SCHEDULER_PARAM_VAR1"
        |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
        |""".stripMargin),
    Map("VAR1" → "FROM-JOB"))
  private val TestJournalMeta = new JsonJournalMeta[OrderEvent](
    snapshotJsonFormat = TypedJsonFormat[Any](Subtype[Order[Order.State]]),
    eventJsonFormat = KeyedEvent.typedJsonFormat[OrderEvent](KeyedSubtype[OrderEvent]))
  private implicit val TestAkkaTimeout = Timeout(99.seconds)

  private final class TestActor(dir: Path, terminatedPromise: Promise[Completed])
  extends Actor {
    private implicit val timerService = TimerService(idleTimeout = Some(1.s))
    private val journalFile = dir / "data" / "state" / "journal"
    private val keyedEventBus = new StampedKeyedEventBus
    private implicit val taskRunnerFactory: TaskRunner.Factory = new SimpleShellTaskRunner.Factory(
      new AgentTaskId.Generator,
      new StandardRichProcessStartSynchronizer()(context),
      AgentConfiguration.forTest(configAndData = Some(dir)))

    private val journalActor = context.actorOf(
      Props {
        new JsonJournalActor[OrderEvent](TestJournalMeta, journalFile, syncOnCommit = false, new EventIdGenerator, keyedEventBus)
      },
      "Journal")
    private val jobActor = JobRunner.actorOf(TestJobPath)
    private val orderActor = context.actorOf(Props { new OrderActor(TestOrder.id, journalActor = journalActor)}, TestOrder.id.string)

    private val orderChangeds = mutable.Buffer[OrderActor.Output.OrderChanged]()
    private val events = mutable.Buffer[OrderEvent]()
    private val stdoutStderr = (for (t ← StdoutStderrType.values) yield t → new StringBuilder).toMap

    keyedEventBus.subscribe(self, classOf[OrderEvent])
    (journalActor ? JsonJournalActor.Input.StartWithoutRecovery).mapTo[JsonJournalActor.Output.Ready.type] await 99.s
    (jobActor ? JobRunner.Command.StartWithConfiguration(TestJobConfiguration)).mapTo[JobRunner.Response.Ready.type] await 99.s
    jobActor ! JobRunner.Input.OrderAvailable

    override def postRestart(t: Throwable) = terminatedPromise.failure(t)

    def receive = {
      case JobRunner.Output.ReadyForOrder ⇒  // JobRunner has sent this to its parent (that's me) in response to OrderAvailble
        orderActor ! OrderActor.Command.Attach(TestOrder)
        context.become(attaching)
    }

    private def attaching: Receive = handleOrderEvent orElse {
      case Completed ⇒
        orderActor ! OrderActor.Input.StartStep(TestJobNode, jobActor = jobActor)
        context.become(ready)
    }

    private def ready: Receive = handleOrderEvent orElse {
      case o: OrderActor.Output.OrderChanged ⇒
        orderChangeds += o
    }

    private def handleOrderEvent: Receive = {
      case Stamped(_, KeyedEvent(TestOrder.id, event: OrderEvent)) ⇒
        event match {
          case OrderStdWritten(t, chunk) ⇒
            assert(events.last == OrderStepStarted)
            stdoutStderr(t) ++= chunk

          case _: OrderStepEnded ⇒
            events += event
            orderActor ! OrderActor.Command.Detach

          case OrderDetached ⇒
            events += event
            assert(events == ExpectedOrderEvents)
            val nl = System.lineSeparator
            assert(stdoutStderr(Stdout).toString == s"Hej!${nl}var1=FROM-JOB$nl")
            assert(stdoutStderr(Stderr).toString == s"THIS IS STDERR$nl")
            terminatedPromise.success(Completed)

          case _ ⇒
            events += event
        }
      }
  }
}
