package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, Akkas}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonFormat
import com.sos.jobscheduler.agent.scheduler.job.task.{SimpleShellTaskRunner, TaskRunner}
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobRunner, JobScript}
import com.sos.jobscheduler.agent.scheduler.order.OrderActorTest._
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
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
import com.typesafe.config.Config
import java.nio.file.Path
import java.time.Instant.now
import org.scalatest.Assertions._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

/**
  * @author Joacim Zschimmer
  */
final class OrderActorTest extends FreeSpec with HasCloser with BeforeAndAfterAll {

  private lazy val directoryProvider = new AgentDirectoryProvider {}
  private lazy val agentConfiguration = AgentConfiguration.forTest(Some(directoryProvider.agentDirectory))
  private lazy val actorSystem = Akkas.newActorSystem("OrderActorTest")

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.provideAgent2Directories()
  }

  override def afterAll() = {
    close()
    directoryProvider.close()
    super.afterAll()
  }

  "Shell script" in {
    val (testActor, result) = runTestActor(TestJobConfiguration)
    assert(result.events == ExpectedOrderEvents)
    assert(result.stdoutStderr(Stdout).toString == s"Hej!${Nl}var1=FROM-JOB$Nl")
    assert(result.stdoutStderr(Stderr).toString == s"THIS IS STDERR$Nl")
    testActor ! PoisonPill
  }

  "Shell script with big stdout and stderr" in {
    val n = 1000
    def line(x: String, i: Int) = (s" $x$i" * ((i+n/100-1)/(n/100))).trim ensuring { _.length < 8000 }  // Windows: Maximum command line length is 8191 characters
    val expectedStderr = (for (i ← 1 to n) yield line("e", i) + Nl).mkString
    val expectedStdout = (for (i ← 1 to n) yield line("o", i) + Nl).mkString
    val jobConfiguration = JobConfiguration(TestJobPath,
      JobScript(
        (if (isWindows) "@echo off\n" else "") +
        (for (i ← 1 to n) yield
          s"""echo ${line("o", i)}
             |echo ${line("e", i)}>&2
             |""".stripMargin).mkString))
    val t = now
    val (testActor, result) = runTestActor(jobConfiguration)
    info(s"2×($n unbuffered lines, ${toKBGB(expectedStdout.length)} took ${(now - t).pretty}")
    assert(result.stdoutStderr(Stderr).toString == expectedStderr)
    assert(result.stdoutStderr(Stdout).toString == expectedStdout)
    testActor ! PoisonPill
  }

  private def runTestActor(jobConfiguration: JobConfiguration): (ActorRef, Result) = {
    val terminatedPromise = Promise[Result]()
    val testActor = actorSystem.actorOf(Props { new TestActor(directoryProvider.agentDirectory, jobConfiguration, terminatedPromise, agentConfiguration.config) }, "TestActor")
    val result: Result = terminatedPromise.future await 99.s
    (testActor, result)
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
  private val Nl = System.lineSeparator

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

  private case class Result(events: Seq[OrderEvent], stdoutStderr: Map[StdoutStderrType, String])



  private final class TestActor(dir: Path, jobConfiguration: JobConfiguration, terminatedPromise: Promise[Result], config: Config)
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
        new JsonJournalActor[OrderEvent](TestJournalMeta, journalFile, syncOnCommit = true, new EventIdGenerator, keyedEventBus)
      },
      "Journal")
    private val jobActor = JobRunner.actorOf(TestJobPath)
    private val orderActor = context.actorOf(Props { new OrderActor(TestOrder.id, journalActor = journalActor, config)}, TestOrder.id.string)

    private val orderChangeds = mutable.Buffer[OrderActor.Output.OrderChanged]()
    private val events = mutable.Buffer[OrderEvent]()
    private val stdoutStderr = (for (t ← StdoutStderrType.values) yield t → new StringBuilder).toMap
    private var detached = false

    keyedEventBus.subscribe(self, classOf[OrderEvent])
    (journalActor ? JsonJournalActor.Input.StartWithoutRecovery).mapTo[JsonJournalActor.Output.Ready.type] await 99.s
    (jobActor ? JobRunner.Command.StartWithConfiguration(jobConfiguration)).mapTo[JobRunner.Response.Ready.type] await 99.s
    jobActor ! JobRunner.Input.OrderAvailable

    override def postRestart(t: Throwable) = terminatedPromise.failure(t)

    def receive = {
      case JobRunner.Output.ReadyForOrder ⇒  // JobRunner has sent this to its parent (that's me) in response to OrderAvailable
        orderActor ! OrderActor.Command.Attach(TestOrder)
        context.become(attaching)
    }

    private def attaching: Receive = receiveOrderEvent orElse {
      case Completed ⇒
        orderActor ! OrderActor.Input.StartStep(TestJobNode, jobActor = jobActor)
        context.become(ready)
    }

    private def ready: Receive = receiveOrderEvent

    private def detaching: Receive = receiveOrderEvent orElse {
      case Completed ⇒
        detached = true
        checkTermination()

      case JobRunner.Output.ReadyForOrder ⇒  // Ready for next order
    }

    private def receiveOrderEvent: Receive = {
      case o: OrderActor.Output.OrderChanged ⇒
        orderChangeds += o
        checkTermination()

      case Stamped(_, KeyedEvent(TestOrder.id, event: OrderEvent)) ⇒  // Duplicate to OrderChangsed, in unknown order
        event match {
          case OrderStdWritten(t, chunk) ⇒
            assert(events.last == OrderStepStarted)
            stdoutStderr(t) ++= chunk

          case _: OrderStepEnded ⇒
            events += event
            orderActor ! OrderActor.Command.Detach
            context.become(detaching)

          case OrderDetached ⇒
            events += event
            checkTermination()

          case _ ⇒
            events += event
        }
      }

    private def checkTermination(): Unit = {
      if (detached && events.lastOption == Some(OrderDetached) && (orderChangeds.lastOption map { _.event }) ==  Some(OrderDetached)) {
        assert(events == (orderChangeds map { _.event }))
        terminatedPromise.success(Result(events.toVector, stdoutStderr mapValues { _.toString }))
        context.stop(self)
      }
    }
  }
}
