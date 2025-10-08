package js7.agent

import cats.effect.Resource.ExitCase
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO, Sync, kernel}
import cats.syntax.flatMap.*
import js7.agent.RunningAgent.TestWiring
import js7.agent.TestAgent.*
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.ShutDown
import js7.agent.motor.AgentCommandExecutor
import js7.base.auth.SessionToken
import js7.base.catsutils.{OurIORuntime, OurIORuntimeRegister}
import js7.base.eventbus.StandardEventBus
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Checked
import js7.base.service.{MainService, Service}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.CatsUtils.syntax.{RichResource, logWhenItTakesLonger}
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.utils.{Allocated, ProgramTermination}
import js7.base.web.Uri
import js7.common.system.startup.ServiceMain
import js7.core.command.CommandMeta
import js7.journal.watch.StrictEventWatch
import js7.subagent.Subagent
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

final class TestAgent(
  allocated: Allocated[IO, RunningAgent],
  terminateProcessesWith: Option[ProcessSignal] = None)
extends
  MainService, Service.StoppableByRequest:

  protected type Termination = DirectorTermination

  val agent = allocated.allocatedThing
  @volatile private var released = false

  private val ioRuntime = allocated.allocatedThing.ioRuntime

  protected def start: IO[Service.Started] =
    startService:
      IO.race(untilStopRequested, untilTerminated) *>
        stopThis

  def untilTerminated: IO[DirectorTermination] =
    agent.untilTerminated <*
      stopThis/*release outer resources*/

  // TODO Make private and use the Service resource mechanism
  def stopThis: IO[Unit] =
    agent.terminate(terminateProcessesWith).void
      .guarantee:
        logger.traceIO("allocated.release"):
          // Stops IORuntime, too
          allocated.release *> IO:
            released = true

  def kill: IO[ProgramTermination] =
    logger.infoIO:
      terminate(
        processSignal = Some(SIGKILL),
        clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))

  def terminate(
    processSignal: Option[ProcessSignal] = None,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false,
    restartDirector: Boolean = false)
  : IO[ProgramTermination] =
    IO.defer:
      if released then
        // The Agent's own test IORuntime has been shutdown
        IO.pure(ProgramTermination())
      else
        logger.traceIO("terminate", s"${Seq(processSignal, clusterAction).flatten.mkString(" ")}"):
          // Stops IORuntime, too
          agent.terminate(
            processSignal, clusterAction,
            suppressSnapshot = suppressSnapshot,
            restartDirector = restartDirector)
        .evalOn(ioRuntime.compute)
        .guarantee:
          stopThis.logWhenItTakesLonger("terminate -> stopThis")

  /** Use TestAgent once, then stop it. */
  def useSync[R](timeout: FiniteDuration)(body: TestAgent => R)(using IORuntime): R =
    logger.debugCall[R](s"useSync »${agent.conf.name}«")(
      allocated.useSync(timeout)(_ => body(this)))

  implicit def actorSystem: ActorSystem =
    agent.actorSystem

  def localUri: Uri =
    agent.localUri

  def sessionToken: SessionToken =
    agent.systemSessionToken

  def untilReady: IO[Checked[AgentCommandExecutor]] =
    agent.untilReady

  def currentAgentState(): AgentState =
    given IORuntime = agent.ioRuntime
    agent.agentState.await(99.s).orThrow

  val eventWatch: StrictEventWatch =
    agent.eventWatch

  export eventWatch.{await, awaitKey, awaitKeys, awaitNext, awaitNextKey, eventsByKey, expect, resetLastWatchedEventId}

  def testEventBus: StandardEventBus[Any] =
    agent.testEventBus

  def executeCommandAsSystemUser(command: AgentCommand): IO[Checked[AgentCommand.Response]] =
    agent.executeCommandAsSystemUser(command)
      .evalOn(ioRuntime.compute)

  def executeCommand(cmd: AgentCommand, meta: CommandMeta): IO[Checked[AgentCommand.Response]] =
    agent.executeCommand(cmd: AgentCommand, meta)
      .evalOn(ioRuntime.compute)

  def name: String =
    conf.name

  def conf: AgentConfiguration =
    agent.conf

  override def toString = s"TestAgent($name)"


object TestAgent:
  private val logger = Logger[this.type]

  def apply(
    allocated: Allocated[IO, RunningAgent],
    terminateProcessesWith: Option[ProcessSignal] = None)
  : TestAgent =
    new TestAgent(allocated, terminateProcessesWith)

  // TODO Prefer the Service resource mechanism below
  def start(
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty,
    subagentTestWiring: Subagent.TestWiring = Subagent.TestWiring.empty,
    terminateProcessesWith: Option[ProcessSignal] = None)
  : IO[TestAgent] =
    CorrelId.bindNew:
      ioRuntimeResource[IO](conf)
        .flatTap: ioRuntime =>
          OurIORuntimeRegister.toEnvironment(ioRuntime)
            .registerMultiple(testWiring.envResources)
        .flatMap: ioRuntime =>
          given IORuntime = ioRuntime
          RunningAgent.withSubagent(conf, testWiring, subagentTestWiring).evalOn(ioRuntime.compute)
        .toAllocated
        .map(new TestAgent(_, terminateProcessesWith))

  def blockingRun(
    conf: AgentConfiguration,
    timeout: FiniteDuration = 99.s,
    terminateProcessesWith: Option[ProcessSignal] = None)
    (body: TestAgent => Unit)
    (using ioRuntime: IORuntime)
  : ProgramTermination =
    ServiceMain.blockingRun(timeout = timeout)(
      resource = resource(conf).evalOn(ioRuntime.compute),
      use = (testAgent: TestAgent) =>
        try body(testAgent)
        catch case NonFatal(t) =>
          logger.debug(s"💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
          try testAgent.terminate(terminateProcessesWith).await(99.s)
          catch case NonFatal(t2) => logger.error(
            s"agent.terminate while handling an exception: ${t2.toStringWithCauses}",
            t2.nullIfNoStackTrace)
          throw t
        testAgent.terminate(terminateProcessesWith).await(99.s))

  def resource(
    conf: AgentConfiguration,
    terminateProcessesWith: Option[ProcessSignal] = None,
    testWiring: TestWiring = TestWiring.empty,
    subagentTestWiring: Subagent.TestWiring = Subagent.TestWiring.empty)
    (using ioRuntime: IORuntime)
  : ResourceIO[TestAgent] =
    RunningAgent.withSubagent(conf, testWiring, subagentTestWiring)
      .flatMap: agent =>
        Resource.makeCase(
          acquire = IO(new TestAgent(new Allocated(agent, agent.terminate().void))))(
          release = (agent, exitCase) => IO.defer:
            exitCase match
              case ExitCase.Errored(t) =>
                logger.error(s"💥 ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                agent.terminate(terminateProcessesWith)
                  .void
                  .handleErrorWith: t =>
                    IO(logger.error(
                      s"agent.terminate while handling an exception: ${t.toStringWithCauses}",
                      t.nullIfNoStackTrace))
              case _ =>
                agent.terminate(terminateProcessesWith)
                  .void
                  .handleErrorWith: t =>
                    IO(logger.error(
                      s"💥 agent.terminate: ${t.toStringWithCauses}",
                      t.nullIfNoStackTrace)))

  private def ioRuntimeResource[F[_]](conf: AgentConfiguration)(implicit F: Sync[F])
  : Resource[F, IORuntime] =
    OurIORuntime.resource[F](conf.name, conf.config)
