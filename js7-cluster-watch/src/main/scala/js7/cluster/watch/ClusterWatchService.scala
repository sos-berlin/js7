package js7.cluster.watch

import cats.effect.Resource
import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.service.StatefulService
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.cluster.watch.ClusterWatchService.*
import js7.cluster.watch.api.ClusterWatchProblems.NoClusterWatchRequestMatches
import js7.common.configuration.Js7Configuration.defaultConfig
import js7.data.cluster.ClusterWatchCommand.ClusterWatchAcknowledge
import js7.data.cluster.{ClusterNodeApi, ClusterWatchCheck, ClusterWatchMessage}
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.Failure

final class ClusterWatchService private[ClusterWatchService](
  nodeApis: Nel[ClusterNodeApi],
  now: () => MonixDeadline,
  heartbeat: FiniteDuration,
  retryDelays: Seq[FiniteDuration])
extends StatefulService.StoppableByRequest
{
  private val clusterWatch = new ClusterWatch(now)
  private val stopNow = PublishSubject[Unit]()

  protected def run: Task[Unit] =
    Task.defer {
      logger.info("Starting ClusterWatch")
      Observable
        .fromIterable(nodeApis.toList
          .map(nodeApi =>
            observeAgainAndAgain(nodeApi)(
              nodeObservable(nodeApi).map(nodeApi -> _))))
        .merge
        .mapEval { case (nodeApi, msg) =>
          handleMessage(nodeApi, msg)
        }
        .takeUntilEval(whenStopRequested)
        .completedL
        .guarantee(Task
          .fromFuture(
            stopNow.onNext(()))
          .void)
    }

  private def nodeObservable(nodeApi: ClusterNodeApi): Observable[ClusterWatchMessage] =
    Observable
      .fromTask(logger.traceTask("nodeObservable", nodeApi)(
        nodeApi
          .retryUntilReachable()(
            nodeApi.retryIfSessionLost()(
              nodeApi.clusterWatchMessageObservable(heartbeat = Some(heartbeat))))
          .materialize.map {
            case Failure(t: HttpException) if t.statusInt == 503 /*Service unavailable*/ =>
              Failure(t) // Trigger onErrorRestartLoop
            case o => HttpClient.failureToChecked(o)
          }
          .dematerialize
          // TODO Similar to ControllerApi and SessionApi
          .onErrorRestartLoop(()) {
            case (t, _, retry) if HttpClient.isTemporaryUnreachable(t) =>
              Task
                .race(
                  whenStopRequested,
                  Task.sleep(1.s/*TODO delays.next()*/))
                .flatMap {
                  case Left(()) => Task.right(Observable.empty)
                  case Right(()) => retry(())
                }
            case (t, _, _) => Task.raiseError(t)
          }
          .map(_.orThrow)))
      .flatten

  private def observeAgainAndAgain[A](nodeApi: ClusterNodeApi)(observable: Observable[A])
  : Observable[A] =
    // TODO Observable.tailRecM: Left leaks memory, https://github.com/monix/monix/issues/791
    Observable.tailRecM(())(after =>
      //if (isStopped)
      //  Observable.pure(Right(Observable.empty))
      //else
        Observable
          .pure(Right(
            observable
              .onErrorHandleWith { t =>
                logger.warn(s"$nodeApi => ${t.toStringWithCauses}")
                Observable.empty[A]
              }))
          .appendAll(
            Observable.evalDelayed(
              1.s/*TODO pauseBeforeNextTry(conf.delay)*/,
              Left(())))
    ).flatten

  private def handleMessage(nodeApi: ClusterNodeApi, msg: ClusterWatchMessage): Task[Unit] =
    msg.correlId.bind(logger.traceTask("handleMessage", msg)(Task.defer(
      clusterWatch.handleMessage(msg).flatMap(checked =>
        msg match {
          case msg: ClusterWatchCheck =>
            // Retry on error ???
            HttpClient
              .liftProblem(nodeApi
                .executeClusterWatchCommand(
                  ClusterWatchAcknowledge(msg.requestId, checked.left.toOption))
                .void)
              .map {
                case Left(problem @ NoClusterWatchRequestMatches) =>
                  logger.info(s"$nodeApi $problem")
                case Left(problem) => logger.warn(s"$nodeApi $problem")
                case Right(()) =>
              }
              .onErrorHandle(t =>
                logger.error(s"$nodeApi ${t.toStringWithCauses}", t.nullIfNoStackTrace))
          case _ =>
            for (problem <- checked.left) logger.warn(s"$nodeApi $problem — msg=$msg")
            Task.unit
        }))))

  override def toString = "ClusterWatchService"
}

object ClusterWatchService
{
  private val logger = Logger(getClass)
  private val errorDelays = Seq(1.s, 3.s, 6.s, 10.s)

  def resource(
    apiResources: Resource[Task, Nel[ClusterNodeApi]],
    config: Config)
  : Resource[Task, ClusterWatchService] = {
    val myConfig = config.withFallback(defaultConfig)
    Resource.suspend(Task {
      val heartbeat = myConfig.finiteDuration("js7.web.client.heartbeat").orThrow
      val retryDelays = myConfig.getDurationList("js7.journal.cluster.watch.retry-delays")
        .asScala.map(_.toFiniteDuration).toVector
      apiResources
        .flatMap(nodeApis =>
          StatefulService.resource(
            Task.deferAction(scheduler => Task(
              new ClusterWatchService(
                nodeApis,
                () => scheduler.now,
                heartbeat = heartbeat,
                retryDelays = retryDelays)))))
    })
  }
}
