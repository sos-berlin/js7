package js7.common.http

import cats.syntax.apply._
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.session.SessionApi
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.http.RecouplingStreamReader._
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.event.EventSeqTornProblem
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import monix.execution.exceptions.UpstreamTimeoutException
import monix.reactive.Observable
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/** Logs in, couples and fetches objects from a (HTTP) stream, and recouples after error. */
abstract class RecouplingStreamReader[
  @specialized(Long/*EventId or file position*/) I,
  V,
  Api <: SessionApi.HasUserAndPassword with HasIsIgnorableStackTrace
](toIndex: V => I,
  conf: RecouplingStreamReaderConf)
{
  protected def couple(index: I): Task[Checked[I]] =
    Task.pure(Right(index))

  protected def getObservable(api: Api, after: I): Task[Checked[Observable[V]]]

  protected def onCouplingFailed(api: Api, problem: Problem): Task[Boolean] =
    inUseVar.flatMap(_.tryRead).map(_.isDefined)
      .flatMap(inUse =>
        Task {
          var logged = false
          lazy val msg = s"$api: coupling failed: $problem"
          if (inUse && !stopRequested && !coupledApiVar.isStopped) {
            logger.warn(msg)
            logged = true
          }
          for (throwable <- problem.throwableOption.map(_.nullIfNoStackTrace)
               if !api.isIgnorableStackTrace(throwable)) {
            logger.debug(msg, throwable)
            logged = true
          }
          if (!logged) {
            logger.debug(s"💥 $api: $msg")
          }
          true  // Recouple and continue
        })

  protected def onCoupled(api: Api, after: I): Task[Completed] =
    Task.completed

  protected def onDecoupled: Task[Completed] =
    Task.completed

  protected def eof(index: I) = false

  // TODO Genügt nicht `terminate` ?
  protected def stopRequested: Boolean

  protected def requestTimeout = conf.timeout

  protected def idleTimeout = Option(requestTimeout + 2.s)/*let service timeout kick in first*/

  private def isStopped = stopRequested || coupledApiVar.isStopped || !inUse.get()

  private val coupledApiVar = new CoupledApiVar[Api]
  private val recouplingPause = new RecouplingPause
  private val inUse = AtomicBoolean(false)
  private val inUseVar = MVar.empty[Task, Unit]().memoize
  private var sinceLastTry = now - 1.hour

  /** Observes endlessly, recoupling and repeating when needed. */
  final def observe(api: Api, after: I): Observable[V] =
    Observable.fromTask(
      Task(logger.debug(s"$api: observe(after=$after)")) *>
        inUseVar.flatMap(_.put(()))
          .*>(Task { inUse := true })
          .logWhenItTakesLonger
          .*>(decouple)
    ) *>
      new ForApi(api, after)
        .observeAgainAndAgain
        .guarantee(Task.defer {
          logger.trace(s"$api: inUse := false")
          inUse := false
          inUseVar.flatMap(_.tryTake.void)
        })

  final def terminateAndLogout: Task[Completed] =
    decouple
      .*>(coupledApiVar.terminate)
      .logWhenItTakesLonger

  final def decouple: Task[Completed] =
    coupledApiVar.isTerminated.flatMap(
      if (_)
        Task.completed
      else
        coupledApiVar.tryTake
          .flatMap {
            case None => Task.completed
            case Some(api) => onDecoupled *> api.tryLogout
          })

  final def invalidateCoupledApi: Task[Completed] =
    coupledApiVar.invalidate

  final def coupledApi: Task[Option[Api]] =
    coupledApiVar.tryRead

  final def pauseBeforeNextTry(delay: FiniteDuration): Task[Unit] =
    Task.defer {
      Task.sleep((sinceLastTry + delay).timeLeftOrZero roundUpToNext PauseGranularity)
    } *>
      Task {
        sinceLastTry = now  // update asynchronously
      }

  private final class ForApi(api: Api, initialAfter: I) {
    @volatile private var lastIndex = initialAfter

    def observeAgainAndAgain: Observable[V] =
      Observable.tailRecM(initialAfter)(after =>
        if (eof(after) || isStopped)
          Observable.pure(Right(Observable.empty))
        else {
          Observable
            .pure(Right(
              observe(after)
                .onErrorHandleWith {
                  case t: ProblemException if t.problem is EventSeqTornProblem =>
                    Observable.raiseError(t)
                  case t =>
                    Observable.fromTask(
                      onCouplingFailed(api, Problem.fromThrowable(t)) map {
                        case false => Observable.raiseError(t)
                        case true => Observable.empty[V]
                      }).flatten
                })) ++
              (Observable.fromTask(pauseBeforeNextTry(conf.delay)) *>
                Observable.eval(Left(lastIndex)/*FIXME Observable.tailRecM: Left leaks memory, https://github.com/monix/monix/issues/791*/))
        }
      ).flatten

    private def observe(after: I): Observable[V] =
      Observable.fromTask(tryEndlesslyToGetObservable(after))
        .flatten
        .map { v =>
          lastIndex = toIndex(v)
          v
        }

    /** Retries until web request returns an Observable. */
    private def tryEndlesslyToGetObservable(after: I): Task[Observable[V]] =
      Task.tailRecM(())(_ =>
        if (isStopped)
          Task.pure(Right(Observable.raiseError(
            new IllegalStateException(s"RecouplingStreamReader($api) has been stopped"))))
        else
          coupleIfNeeded(after = after)
            .flatMap(after => /*`after` may have changed after initial AgentDedicated.*/
              getObservableX(after = after)
                .materialize.map(Checked.flattenTryChecked)
                .flatMap {
                  case Left(problem) =>
                    if (isStopped)
                      Task.pure(Left(()))  // Fail in next iteration
                    else if (problem is EventSeqTornProblem)
                      Task.raiseError(problem.throwable)
                    else
                      (problem match {
                        case InvalidSessionTokenProblem =>
                          Task {
                            logger.debug(s"⛔️ $api: $InvalidSessionTokenProblem")
                            true
                          }
                        case _ =>
                          onCouplingFailed(api, problem)
                      }).flatMap(continue =>
                        decouple *>
                          (if (continue)
                            pauseBeforeRecoupling.as(Left(()))
                          else
                            Task.right(Observable.empty)))

                  case Right(observable) =>
                    Task.right(observable)
                }))

    private def getObservableX(after: I): Task[Checked[Observable[V]]] =
      Task {
        sinceLastTry = now
      } *>
        getObservable(api, after = after)
          //.timeout(idleTimeout)
          .onErrorRecoverWith { case t: TimeoutException =>
            logger.debug(s"💥 $api: ${t.toString}")
            Task.pure(Right(Observable.empty))
          }
          .map(_.map(obs =>
            idleTimeout.fold(obs)(idleTimeout => obs
              .timeoutOnSlowUpstream(idleTimeout)  // cancels upstream!
              .onErrorRecoverWith { case t: UpstreamTimeoutException =>
                logger.debug(s"💥 $api: ${t.toString}")
                // This should let Akka close the TCP connection to abort the stream
                Observable.empty
              })))

    private def coupleIfNeeded(after: I): Task[I] =
      coupledApiVar.tryRead.flatMap {
        case Some(_) => Task.pure(after)
        case None => tryEndlesslyToCouple(after)
      }

    private def tryEndlesslyToCouple(after: I): Task[I] =
      Task.tailRecM(())(_ => Task.defer(
        if (isStopped)
          Task.raiseError(new IllegalStateException(s"RecouplingStreamReader($api) has been stopped")
            with NoStackTrace)
        else
          ( for {
              otherCoupledClient <- coupledApiVar.tryRead
              _ <- otherCoupledClient.fold(Task.unit)(_ =>
                Task.raiseError(new IllegalStateException("Coupling while already coupled")))
              _ <- Task { recouplingPause.onCouple() }
              _ <- api.login(onlyIfNotLoggedIn = true)//.timeout(idleTimeout)
              updatedIndex <- couple(index = after) /*AgentDedicated may return a different EventId*/
            } yield updatedIndex
          ) .materializeIntoChecked
            .flatMap {
              case Left(problem) =>
                if (isStopped)
                  Task.pure(Left(()))  // Fail in next iteration
                else
                  for {
                    _ <- Task.when(problem == InvalidSessionTokenProblem)(
                      api.tryLogout.void)
                    // TODO akka.stream.scaladsl.TcpIdleTimeoutException sollte still ignoriert werden, ist aber abhängig von Akka
                    continue <- onCouplingFailed(api, problem)
                    either <-
                      if (continue)
                        pauseBeforeRecoupling.map(_ => Left(()))
                      else
                        Task.raiseError(problem.throwable)
                  } yield either

              case Right(updatedIndex) =>
                for {
                  _ <- coupledApiVar.put(api)
                  _ <- Task { recouplingPause.onCouplingSucceeded() }
                  _ <- onCoupled(api, after)
                } yield Right(updatedIndex)
            }))
  }

  private val pauseBeforeRecoupling =
    Task.defer(pauseBeforeNextTry(recouplingPause.nextPause()))
}

object RecouplingStreamReader
{
  val TerminatedProblem = Problem.pure("RecouplingStreamReader has been stopped")

  private val PauseGranularity = 500.ms
  private val logger = scribe.Logger[this.type]

  def observe[@specialized(Long/*EventId or file position*/) I, V, Api <: SessionApi.HasUserAndPassword with HasIsIgnorableStackTrace](
    toIndex: V => I,
    api: Api,
    conf: RecouplingStreamReaderConf,
    after: I,
    getObservable: I => Task[Checked[Observable[V]]],
    eof: I => Boolean = (_: I) => false,
    stopRequested: () => Boolean = () => false)
  : Observable[V]
  = {
    val eof_ = eof
    val getObservable_ = getObservable
    val stopRequested_ = stopRequested
    new RecouplingStreamReader[I, V, Api](toIndex, conf) {
      def getObservable(api: Api, after: I) = getObservable_(after)
      override def eof(index: I) = eof_(index)
      def stopRequested = stopRequested_()
    }.observe(api, after)
  }

  private class RecouplingPause {
    // This class may be used asynchronously but not concurrently
    private val Minimum = 1.s
    @volatile private var pauses = initial
    @volatile private var lastCouplingTriedAt = now

    def onCouple() = lastCouplingTriedAt = now
    def onCouplingSucceeded() = pauses = initial
    def nextPause() = (lastCouplingTriedAt + synchronized { pauses.next() }).timeLeft max Minimum

    private def initial = Iterator(Minimum, 1.s, 1.s, 1.s, 2.s, 5.s) ++
      Iterator.continually(10.s)
  }
}
