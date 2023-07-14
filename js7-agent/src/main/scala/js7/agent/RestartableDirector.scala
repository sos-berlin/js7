package js7.agent

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.implicits.toFlatMapOps
import js7.agent.RunningAgent.TestWiring
import js7.agent.configuration.AgentConfiguration
import js7.base.monixutils.AsyncVariable
import js7.base.service.{MainService, Service}
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.RichDeferred
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.subagent.Subagent
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.{Failure, Success, Try}

final class RestartableDirector private(
  subagent: Subagent,
  conf: AgentConfiguration,
  testWiring: TestWiring = TestWiring.empty)
  (implicit scheduler: Scheduler)
extends MainService with Service.StoppableByRequest
{
  protected type Termination = DirectorTermination

  private val _currentDirector = AsyncVariable[RunningAgent](null: RunningAgent)
  private val _untilTerminated = Deferred.unsafe[Task, Try[DirectorTermination]]

  protected def start =
    RunningAgent
      .director(subagent, conf, testWiring)
      .toAllocated
      .flatMap(allocated =>
        _currentDirector.set(allocated.allocatedThing) *>
          startService(
            onStopRequested(allocated.release)
              .use(_ =>
                allocated.allocatedThing.untilTerminated)
              .guarantee(allocated.release)
              .flatMap(terminated =>
                if (terminated.restart)
                  restartLoop
                else
                  Task.pure(terminated))
              .flatTap(termination =>
                _untilTerminated.complete(Success(termination)).as(Right(())))
              .tapError(t => // ???
                _untilTerminated.complete3(Failure(t)).void)
              .void))

  private def restartLoop: Task[DirectorTermination] =
    Task.tailRecM(())(_ => RunningAgent
      .director(subagent, conf, testWiring)
      .use(director =>
        onStopRequested(director.terminate().void)
          .use(_ =>
            _currentDirector.set(director) *>
              director.untilTerminated)
          .flatMap(termination =>
            if (termination.restart)
              Task.left(())
            else
              Task.sleep(1.s/*some time for Akka Actors to terminate and release their names*/)
                .as(Right(termination)))))

  private def onStopRequested(stop: Task[Unit]): Resource[Task, Unit] =
    Resource
      .make(
        acquire = untilStopRequested.*>(stop).start)(
        release = _.cancel)
      .map(_ => ())

  def untilTerminated: Task[DirectorTermination] =
    _untilTerminated.get.dematerialize

  def currentDirector: Task[RunningAgent] =
    _currentDirector.value

  override def toString = "RestartableDirector"
}

private object RestartableDirector {
  def apply(
    subagent: Subagent,
    conf: AgentConfiguration,
    testWiring: TestWiring = TestWiring.empty)
    (implicit scheduler: Scheduler)
  : Resource[Task, RestartableDirector] =
    Service.resource(Task(
      new RestartableDirector(subagent, conf, testWiring)))
}
