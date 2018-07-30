package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.job.task.{SimpleShellTaskRunner, TaskRunner}
import com.sos.jobscheduler.agent.scheduler.job.{JobActor, JobConfiguration, JobScript}
import com.sos.jobscheduler.agent.scheduler.order.OrderActorTest._
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.akkautils.{CatchingActor, SupervisorStrategies}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalActor
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderDetached, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.taskserver.modules.shell.StandardRichProcessStartSynchronizer
import com.typesafe.config.Config
import java.nio.file.{Files, Path}
import java.time.Instant.now
import monix.execution.Scheduler
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

  private lazy val directoryProvider = new TestAgentDirectoryProvider {}
  private lazy val config = AgentConfiguration.forTest(directoryProvider.agentDirectory).finishAndProvideFiles.config
  private lazy val actorSystem = newActorSystem("OrderActorTest")

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
    Files.delete(directoryProvider.dataDirectory / "state/agent--0.journal")
  }

  "Shell script with big stdout and stderr" in {
    val n = 1000
    def line(x: String, i: Int) = (s" $x$i" * ((i+n/100-1)/(n/100))).trim ensuring { _.length < 8000 }  // Windows: Maximum command line length is 8191 characters
    val expectedStderr = (for (i ← 1 to n) yield line("e", i) + Nl).mkString
    val expectedStdout = (for (i ← 1 to n) yield line("o", i) + Nl).mkString
    val jobConfiguration = JobConfiguration(TestJobId,
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

  private var actorCounter = 0

  private def runTestActor(jobConfiguration: JobConfiguration): (ActorRef, Result) = {
    actorCounter += 1
    def props(promise: Promise[Result]) = Props { new TestActor(directoryProvider.agentDirectory, jobConfiguration, promise, config) }
    val (testActor, terminated) = CatchingActor.actorOf(props, s"TestActor-$actorCounter")(actorSystem)
    val result: Result = terminated await 99.s  // Continues when the nested actor has terminated. CatchingActor may still run for some microseconds.
    (testActor, result)
  }
}

private object OrderActorTest {
  private val TestVersion = VersionId("VERSION")
  private val TestOrder = Order(OrderId("TEST-ORDER"), WorkflowPath("/WORKFLOW") % TestVersion, Order.Ready)
  private val TestJobPath = JobPath("/test")
  private val TestJobId = TestJobPath % TestVersion
  private val TestAgentPath = AgentPath("/TEST-AGENT")
  private val TestJob = Job(TestJobPath, TestAgentPath)
  private val TestPosition = Position(777)
  private val ExpectedOrderEvents = List(
    OrderAttached(TestOrder.workflowPosition, Order.Ready, None, AgentPath("/TEST-AGENT") % "(initial)", Payload.empty),
    OrderProcessingStarted,
    OrderProcessed(MapDiff(Map("result" → "TEST-RESULT-FROM-JOB")), Outcome.succeeded),
    OrderMoved(TestPosition),
    OrderDetached)
  private val Nl = System.lineSeparator

  private val TestJobConfiguration = JobConfiguration(TestJobId,
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

  private implicit val TestAkkaTimeout = Timeout(99.seconds)

  private case class Result(events: Seq[OrderEvent], stdoutStderr: Map[StdoutOrStderr, String])



  private final class TestActor(dir: Path, jobConfiguration: JobConfiguration, terminatedPromise: Promise[Result], config: Config)
  extends Actor {
    import context.{actorOf, become}
    override val supervisorStrategy = SupervisorStrategies.escalate
    private implicit val timerService = TimerService(idleTimeout = Some(1.s))
    private val keyedEventBus = new StampedKeyedEventBus
    private val taskRunnerFactory: TaskRunner.Factory = new SimpleShellTaskRunner.Factory(
      new AgentTaskId.Generator,
      new StandardRichProcessStartSynchronizer()(context.system),
      AgentConfiguration.forTest(configAndData = dir))

    private val journalMeta = JournalMeta(
      snapshotJsonCodec = TypedJsonCodec[Any](Subtype[Order[Order.State]]),
      eventJsonCodec = KeyedEvent.typedJsonCodec[OrderEvent](KeyedSubtype[OrderEvent]),
      dir / "data" / "state" / "agent")

    private val journalActor = actorOf(
      JournalActor.props(journalMeta, syncOnCommit = true, keyedEventBus, Scheduler.global),
      "Journal")
    private val jobActor = context.watch(context.actorOf(JobActor.props(TestJobPath, taskRunnerFactory, timerService)))
    private val orderActor = actorOf(Props { new OrderActor(TestOrder.id, journalActor = journalActor, config)}, s"Order-${TestOrder.id.string}")

    private val orderChangeds = mutable.Buffer[OrderActor.Output.OrderChanged]()
    private val events = mutable.Buffer[OrderEvent]()
    private val stdoutStderr = (for (t ← StdoutOrStderr.values) yield t → new StringBuilder).toMap
    private var jobActorTerminated = false

    keyedEventBus.subscribe(self, classOf[OrderEvent])
    (journalActor ? JournalActor.Input.StartWithoutRecovery) pipeTo self

    def receive = {
      case JournalActor.Output.Ready ⇒
        become(journalReady)
        (jobActor ? JobActor.Command.StartWithConfiguration(jobConfiguration)) pipeTo self
    }

    private def journalReady: Receive = {
      case JobActor.Response.Ready ⇒
        become(jobActorReady)
        jobActor ! JobActor.Input.OrderAvailable
    }

    private def jobActorReady: Receive = {
      case JobActor.Output.ReadyForOrder ⇒  // JobActor has sent this to its parent (that's me) in response to OrderAvailable
        orderActor ! OrderActor.Command.Attach(TestOrder.copy(
          attachedTo = Some(Order.AttachedTo.Agent(TestAgentPath % "(initial)"))))
        become(attaching)
    }

    private def attaching: Receive = receiveOrderEvent orElse {
      case Completed ⇒
        orderActor ! OrderActor.Input.StartProcessing(TestJob, jobActor = jobActor)
        become(ready)
    }

    private def ready: Receive = receiveOrderEvent orElse {
      case JobActor.Output.ReadyForOrder ⇒  // Ready for next order
    }

    private def detaching: Receive = receiveOrderEvent orElse {
      case Completed ⇒
        jobActor ! AgentCommand.Terminate(sigkillProcessesAfter = Some(0.seconds))
        become(terminatingJobActor)

      case JobActor.Output.ReadyForOrder ⇒  // Ready for next order
    }

    private def terminatingJobActor: Receive = receiveOrderEvent orElse {
      case Terminated(`jobActor`) ⇒
        jobActorTerminated = true
        checkTermination()
    }

    private def receiveOrderEvent: Receive = {
      case o: OrderActor.Output.OrderChanged ⇒
        orderChangeds += o
        checkTermination()

      case Stamped(_, _, KeyedEvent(TestOrder.id, event: OrderEvent)) ⇒  // Duplicate to OrderChanged, in unknown order
        event match {
          case OrderStdWritten(t, chunk) ⇒
            assert(events.last == OrderProcessingStarted)
            stdoutStderr(t) ++= chunk

          case _: OrderProcessed ⇒
            events += event
            orderActor ! OrderActor.Input.HandleEvent(OrderMoved(TestPosition))

            case _: OrderMoved ⇒
              events += event
            orderActor ! OrderActor.Command.Detach
            become(detaching)

          case OrderDetached ⇒
            events += event
            checkTermination()

          //TODO case OrderStopped | OrderResumed

          case _ ⇒
            events += event
        }
      }

    private def checkTermination(): Unit = {
      if (jobActorTerminated && events.lastOption.contains(OrderDetached) && (orderChangeds.lastOption map { _.event } contains OrderDetached)) {
        assert(events == (orderChangeds map { _.event }))
        terminatedPromise.success(Result(events.toVector, stdoutStderr mapValues { _.toString }))
        context.stop(self)
      }
    }
  }
}
