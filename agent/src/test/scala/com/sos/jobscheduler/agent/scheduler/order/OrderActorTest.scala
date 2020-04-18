package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.jobscheduler.agent.AgentState
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newAgentActorSystem
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.{SimpleShellTaskRunner, TaskRunner}
import com.sos.jobscheduler.agent.scheduler.order.OrderActorTest._
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.HasCloser
import com.sos.jobscheduler.common.akkautils.{CatchingActor, SupervisorStrategies}
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.utils.ByteUnits.toKBGB
import com.sos.jobscheduler.common.utils.Exceptions.repeatUntilNoException
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.journal.{JournalActor, JournalConf}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.{ExecutablePath, JobKey}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderDetachable, OrderDetached, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.taskserver.modules.shell.StandardRichProcessStartSynchronizer
import com.typesafe.config.{Config, ConfigValueFactory}
import java.nio.file.{Files, Path}
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertions._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OrderActorTest extends AnyFreeSpec with HasCloser with BeforeAndAfterAll
{
  private lazy val directoryProvider = TestAgentDirectoryProvider()
  private lazy val config = AgentConfiguration.forTest(directoryProvider.agentDirectory).finishAndProvideFiles.config
    .withValue("jobscheduler.journal.simulate-sync", ConfigValueFactory.fromAnyRef("20ms"))
  private lazy val actorSystem = newAgentActorSystem("OrderActorTest")

  override def afterAll() = {
    close()
    directoryProvider.close()
    super.afterAll()
  }

  "Shell script" in {
    val executablePath = ExecutablePath(s"/TEST-1$sh")
    executablePath.toFile(directoryProvider.agentDirectory / "config" / "executables").writeExecutable(TestScript)
    val (testActor, result) = runTestActor(DummyJobKey, WorkflowJob(TestAgentRefPath, executablePath, Map("VAR1" -> "FROM-JOB")))
    assert(result.events == ExpectedOrderEvents)
    assert(result.stdoutStderr(Stdout).toString == s"Hej!${Nl}var1=FROM-JOB$Nl")
    assert(result.stdoutStderr(Stderr).toString == s"THIS IS STDERR$Nl")
    actorSystem.stop(testActor)
    repeatUntilNoException(9.s, 10.ms) {  // Windows
      Files.delete(directoryProvider.dataDirectory / "state/agent--0.journal")
    }
  }

  "Shell script with big stdout and stderr" in {
    val n = 1000
    def line(x: String, i: Int) = (s" $x$i" * ((i+n/100-1)/(n/100))).trim ensuring { _.length < 8000 }  // Windows: Maximum command line length is 8191 characters
    val expectedStderr = (for (i <- 1 to n) yield line("e", i) + Nl).mkString
    val expectedStdout = (for (i <- 1 to n) yield line("o", i) + Nl).mkString
    val executablePath = ExecutablePath(s"/TEST-2$sh")
    executablePath.toFile(directoryProvider.agentDirectory / "config" / "executables").writeExecutable(
      (if (isWindows) "@echo off\n" else "") +
        (for (i <- 1 to n) yield
          s"""echo ${line("o", i)}
             |echo ${line("e", i)}>&2
             |""".stripMargin).mkString)
    val (testActor, result) = runTestActor(DummyJobKey, WorkflowJob(TestAgentRefPath, executablePath))
    info(s"2×($n unbuffered lines, ${toKBGB(expectedStdout.length)}) took ${result.duration.pretty}")
    assert(result.stdoutStderr(Stderr).toString == expectedStderr)
    assert(result.stdoutStderr(Stdout).toString == expectedStdout)
    testActor ! PoisonPill
  }

  private var actorCounter = 0

  private def runTestActor(jobKey: JobKey, workflowJob: WorkflowJob): (ActorRef, Result) = {
    actorCounter += 1
    def props(promise: Promise[Result]) = Props { new TestActor(directoryProvider.agentDirectory, jobKey, workflowJob, promise, config) }
    val (testActor, terminated) = CatchingActor.actorOf(props, s"TestActor-$actorCounter")(actorSystem)
    val result: Result = terminated await 99.s  // Continues when the nested actor has terminated. CatchingActor may still run for some microseconds.
    (testActor, result)
  }
}

private object OrderActorTest {
  private val TestVersion = VersionId("VERSION")
  private val TestOrder = Order(OrderId("TEST-ORDER"), WorkflowPath("/WORKFLOW") ~ TestVersion, Order.Ready)
  private val DummyJobKey = JobKey.Named(WorkflowPath.NoId, WorkflowJob.Name("test"))
  private val TestAgentRefPath = AgentRefPath("/TEST-AGENT")
  private val TestPosition = Position(777)
  private val ExpectedOrderEvents = List(
    OrderAttached(TestOrder.arguments, TestOrder.workflowPosition, Order.Ready, TestOrder.historicOutcomes, None, AgentRefPath("/TEST-AGENT")),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(Map("result" -> "TEST-RESULT-FROM-JOB"))),
    OrderMoved(TestPosition),
    OrderDetachable,
    OrderDetached)
  private val Nl = System.lineSeparator

  private val TestScript =
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
      |""".stripMargin

  private implicit val TestAkkaTimeout = Timeout(99.seconds)

  private case class Result(events: Seq[OrderEvent], stdoutStderr: Map[StdoutOrStderr, String], duration: FiniteDuration)



  private final class TestActor(dir: Path, jobKey: JobKey, workflowJob: WorkflowJob, terminatedPromise: Promise[Result], config: Config)
  extends Actor {
    import context.{actorOf, become, watch}
    override val supervisorStrategy = SupervisorStrategies.escalate
    private val taskRunnerFactory: TaskRunner.Factory = new SimpleShellTaskRunner.Factory(
      new AgentTaskId.Generator,
      new StandardRichProcessStartSynchronizer()(context.system),
      AgentConfiguration.forTest(configAndData = dir))

    private val journalMeta = JournalMeta(
      snapshotJsonCodec = TypedJsonCodec[Any](Subtype[Order[Order.State]]),
      eventJsonCodec = KeyedEvent.typedJsonCodec[Event](KeyedSubtype[OrderEvent]),
      dir / "data" / "state" / "agent")

    private val journalActor = actorOf(
      JournalActor.props[AgentState](journalMeta, JournalConf.fromConfig(config), new StampedKeyedEventBus, Scheduler.global, new EventIdGenerator),
      "Journal")
    private val eventWatch = new JournalEventWatch(journalMeta, config)
    private val jobActor = actorOf(
      JobActor.props(JobActor.Conf(jobKey, workflowJob, taskRunnerFactory,
        temporaryDirectory = dir / "data" / "tmp",
        executablesDirectory = (dir / "config" / "executables").toRealPath(),
        scriptInjectionAllowed = false)))
    private val orderActor = watch(actorOf(
      OrderActor.props(TestOrder.id, journalActor = journalActor, OrderActor.Conf(config)),
      s"Order-${TestOrder.id.string}"))

    private val orderChangeds = mutable.Buffer[OrderActor.Output.OrderChanged]()
    private val events = mutable.Buffer[OrderEvent]()
    private val stdoutStderr = (for (t <- StdoutOrStderr.values) yield t -> new StringBuilder).toMap
    private var orderDetached = false
    private var orderActorTerminated = false

    (journalActor ? JournalActor.Input.StartWithoutRecovery(AgentState.empty, Some(eventWatch))) pipeTo self
    eventWatch.observe(EventRequest.singleClass[OrderEvent](timeout = Some(999.s))) foreach self.!
    val runningSince = now

    override def postStop(): Unit = {
      eventWatch.close()
      super.postStop()
    }

    def receive = {
      case JournalActor.Output.Ready(_) =>
        become(jobActorReady)
        jobActor ! JobActor.Input.OrderAvailable
    }

    private def jobActorReady: Receive = {
      case JobActor.Output.ReadyForOrder =>  // JobActor has sent this to its parent (that's me) in response to OrderAvailable
        orderActor ! OrderActor.Command.Attach(TestOrder.copy(
          attachedState = Some(Order.Attached(TestAgentRefPath))))
        become(attaching)
    }

    private def attaching: Receive = receiveOrderEvent orElse {
      case Completed =>
        orderActor ! OrderActor.Input.StartProcessing(jobKey, workflowJob, defaultArguments = Map.empty, jobActor = jobActor)
        become(processing)
    }

    private def processing: Receive = receiveOrderEvent orElse {
      case JobActor.Output.ReadyForOrder =>  // Ready for next order
    }

    private def detachable: Receive = receiveOrderEvent

    private def detaching: Receive = receiveOrderEvent orElse {
      case "DETACHED" =>
        orderDetached = true
        checkTermination()

      case Terminated(`orderActor`) =>
        orderActorTerminated = true
        checkTermination()

      case JobActor.Output.ReadyForOrder =>  // Ready for next order
    }

    private def terminating: Receive = receiveOrderEvent orElse {
      case Terminated(`orderActor`) =>
        orderActorTerminated = true
        checkTermination()
    }

    private def receiveOrderEvent: Receive = {
      case o: OrderActor.Output.OrderChanged =>
        orderChangeds += o
        checkTermination()

      case Stamped(_, _, KeyedEvent(TestOrder.id, event: OrderEvent)) =>  // Duplicate to OrderChanged, in unknown order
        event match {
          case OrderStdWritten(t, chunk) =>
            assert(events.last == OrderProcessingStarted)
            stdoutStderr(t) ++= chunk

          case _: OrderProcessed =>
            events += event
            orderActor ? OrderActor.Command.HandleEvent(OrderMoved(TestPosition)) await 99.s

          case _: OrderMoved =>
            events += event
            orderActor ? OrderActor.Command.HandleEvent(OrderDetachable) await 99.s
            become(detachable)

          case OrderDetachable =>
            events += event
            (orderActor ? OrderActor.Command.HandleEvent(OrderDetached)).mapTo[Completed] map { _ => self ! "DETACHED" }
            become(detaching)

          case OrderDetached =>
            events += event
            checkTermination()

          case _ =>
            events += event
        }
      }

    private def checkTermination(): Unit = {
      if (orderDetached && orderActorTerminated && events.lastOption.contains(OrderDetached) && (orderChangeds.lastOption map { _.event } contains OrderDetached)) {
        assert(events == (orderChangeds map { _.event }))
        terminatedPromise.success(Result(events.toVector, stdoutStderr.view.mapValues(_.toString).toMap, runningSince.elapsed))
        context.stop(self)
      }
    }
  }
}
