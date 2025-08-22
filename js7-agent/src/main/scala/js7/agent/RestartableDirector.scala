package js7.agent

import cats.effect.{Resource, ResourceIO}
import cats.effect.Deferred
import cats.syntax.flatMap.*
import js7.agent.RestartableDirector.*
import js7.agent.RunningAgent.TestWiring
import js7.agent.configuration.AgentConfiguration
import js7.base.log.Logger
import js7.base.monixutils.AsyncVariable
import js7.base.service.{MainService, Service}
import js7.base.time.ScalaTime.{DurationRichInt, RichDuration}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.common.pekkohttp.web.PekkoWebServer
import js7.subagent.Subagent
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.CatsEffectExtensions.startAndLogError
import js7.base.monixlike.MonixLikeExtensions.{dematerialize, tapError}
import js7.base.utils.Nulls.nullToNone
import scala.util.{Failure, Success, Try}

final class RestartableDirector private(
  subagent: Subagent,
  conf: AgentConfiguration,
  testWiring: TestWiring = TestWiring.empty)
  (using ioRuntime: IORuntime)
extends MainService, Service.StoppableByRequest:

  protected type Termination = DirectorTermination

  private val _currentDirector = AsyncVariable[RunningAgent | Null](null)
  private val _untilTerminated = Deferred.unsafe[IO, Try[DirectorTermination]]

  protected def start =
    RunningAgent
      .director(subagent, conf, testWiring)
      .toAllocated
      .flatMap: allocated =>
        _currentDirector.set(allocated.allocatedThing) *>
          startService:
            onStopRequested(allocated.release)
              .surround:
                allocated.allocatedThing.untilTerminated
              .guarantee(allocated.release)
              .flatMap: terminated =>
                if terminated.restartDirector then
                  restartLoop
                else
                  IO.pure(terminated)
              .flatTap: termination =>
                _untilTerminated.complete(Success(termination)).as(Right(()))
              .tapError: t => // ???
                _untilTerminated.complete(Failure(t)).void
              .void

  private def restartLoop: IO[DirectorTermination] =
    ().tailRecM: _ =>
      IO(logger.info(s"⟲ Restart Agent Director after ${Delay.pretty}...")) *>
        IO.sleep(Delay) *>
        RunningAgent
          .director(subagent, conf, testWiring)
          .use: director =>
            onStopRequested(director.terminate().void)
              .surround:
                _currentDirector.set(director) *>
                  director.untilTerminated
              .map: termination =>
                if termination.restartDirector then
                  Left(())
                else
                  Right(termination)

  private def onStopRequested(stop: IO[Unit]): ResourceIO[Unit] =
    Resource
      .make(
        acquire = (untilStopRequested *> stop).startAndLogError)(
        release = _.cancel)
      .void

  def untilTerminated: IO[DirectorTermination] =
    _untilTerminated.get.dematerialize

  def currentDirector: IO[RunningAgent] =
    _currentDirector.value.map(nullToNone).flatMap:
      case None =>
        IO.raiseError(new IllegalStateException("RestartableDirector.currentDirector: Not started"))
      case Some(o) => IO.pure(o)

  def webServer: PekkoWebServer =
    subagent.webServer

  override def toString = "RestartableDirector"


private object RestartableDirector:
  private val logger = Logger[this.type]
  private val Delay = 1.s // Give Pekko Actors time to terminate and release their names

  def resource(
    subagent: Subagent,
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (using IORuntime)
  : ResourceIO[RestartableDirector] =
    Service.resource:
      new RestartableDirector(subagent, conf, testWiring)
