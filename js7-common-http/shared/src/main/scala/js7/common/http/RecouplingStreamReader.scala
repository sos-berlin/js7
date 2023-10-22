package js7.common.http

import cats.syntax.apply.*
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient.HttpException
import js7.common.http.RecouplingStreamReader.*
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.Problems.AckFromActiveClusterNodeProblem
import js7.data.event.EventSeqTornProblem
import js7.base.utils.MVar
import cats.effect.IO
import js7.base.utils.Atomic
import monix.execution.exceptions.UpstreamTimeoutException
import fs2.Stream
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.util.control.NoStackTrace

/** Logs in, couples and fetches objects from a (HTTP) stream, and recouples after error. */
abstract class RecouplingStreamReader[
  @specialized(Long/*EventId or file position*/) I,
  V,
  Api <: SessionApi.HasUserAndPassword & HasIsIgnorableStackTrace
](toIndex: V => I,
  conf: RecouplingStreamReaderConf):

  private val sym = new BlockingSymbol

  protected def couple(index: I): IO[Checked[I]] =
    IO.right(index)

  protected def getStream(api: Api, after: I): IO[Checked[Stream[IO, V]]]

  protected def onCouplingFailed(api: Api, problem: Problem): IO[Boolean] =
    inUseVar.flatMap(_.tryRead).map(_.isDefined)
      .flatMap(inUse =>
        IO {
          var logged = false
          lazy val msg = s"$api: coupling failed: $problem"
          if inUse && !stopRequested && !coupledApiVar.isStopped then {
            sym.onWarn()
            logger.warn(s"$sym $msg")
            logged = true
          }
          for throwable <- problem.throwableOption.map(_.nullIfNoStackTrace)
               if api.hasRelevantStackTrace(throwable) do {
            logger.debug(s"💥 $msg", throwable)
            logged = true
          }
          if !logged then {
            logger.debug(s"💥 $api: $msg")
          }
          true  // Recouple and continue
        })

  protected def onCoupled(api: Api, after: I): IO[Completed] =
    IO.completed

  protected def onDecoupled: IO[Completed] =
    IO.completed

  protected def eof(index: I) = false

  // TODO Genügt nicht `terminate` ?
  protected def stopRequested: Boolean

  protected def requestTimeout = conf.timeout

  protected def idleTimeout = Option(requestTimeout + 2.s)/*let service timeout kick in first*/

  private def isStopped = stopRequested || coupledApiVar.isStopped || !inUse.get()

  private val coupledApiVar = new CoupledApiVar[Api]
  private val recouplingPause = new RecouplingPause
  private val inUse = Atomic(false)
  private val inUseVar = MVar.empty[IO, Unit]().memoize
  private var sinceLastTry = now - 1.hour

  /** Observes endlessly, recoupling and repeating when needed. */
  final def observe(api: Api, after: I): Stream[IO, V] =
    logger.debugStream("observe", s"$api after=$after")(
      Stream.fromIO(
        inUseVar.flatMap(_.put(()))
          .*>(IO { inUse := true })
          .logWhenItTakesLonger
          .*>(decouple)
      ) *>
        new ForApi(api, after)
          .observeAgainAndAgain
          .guarantee(IO.defer {
            logger.trace(s"$api: inUse := false")
            inUse := false
            inUseVar.flatMap(_.tryTake.void)
          }))

  final def terminateAndLogout: IO[Unit] =
    decouple
      .*>(coupledApiVar.terminate)
      .logWhenItTakesLonger

  final def decouple: IO[Completed] =
    coupledApiVar.isTerminated.flatMap(
      if _ then
        IO.completed
      else
        coupledApiVar.tryTake
          .flatMap {
            case None => IO.completed
            case Some(api) => onDecoupled *> api.tryLogout
          })

  final def invalidateCoupledApi: IO[Completed] =
    coupledApiVar.invalidate

  final def coupledApi: IO[Option[Api]] =
    coupledApiVar.tryRead

  final def pauseBeforeNextTry(delay: FiniteDuration): IO[Unit] =
    IO.defer {
      IO.sleep((sinceLastTry + delay).timeLeftOrZero roundUpToNext PauseGranularity)
    } *>
      IO:
        sinceLastTry = now  // update asynchronously

  private final class ForApi(api: Api, initialAfter: I):
    @volatile private var lastIndex = initialAfter

    def observeAgainAndAgain: Stream[IO, V] =
      Stream.tailRecM(initialAfter)(after =>
        if eof(after) || isStopped then
          Stream.emit(Right(Stream.empty))
        else
          Stream
            .pure(Right(
              streamAfter(after)
                .handleErrorWith {
                  case t: ProblemException if isSevereProblem(t.problem) =>
                    Stream.raiseError(t)
                  case t =>
                    Observable.fromIO(
                    Stream.fromIO(
                      onCouplingFailed(api, Problem.fromThrowable(t)) map {
                        case false => Stream.raiseError(t)
                        case true => Stream.empty[V]
                      }).flatten
                })) ++
              (Observable.fromIO(pauseBeforeNextTry(conf.delay)) *>
                Observable.eval(Left(lastIndex)))
              (Stream.fromIO(pauseBeforeNextTry(conf.delay)) *>
                Stream.eval(Left(lastIndex)))
      ).flatten

    private def streamAfter(after: I): Stream[IO, V] =
      Stream.fromIO(
        tryEndlesslyToGetStream(after)
          .<*(IO {
            if sym.called then logger.info(s"🟢 Observing $api ...")
          }))
        .flatten
        .map { v =>
          lastIndex = toIndex(v)
          v
        }

    /** Retries until web request returns an Stream. */
    private def tryEndlesslyToGetStream(after: I): IO[Stream[IO, V]] =
      IO.tailRecM(())(_ =>
        if isStopped then
          IO.right(Stream.raiseError(
            new IllegalStateException(s"RecouplingStreamReader($api) has been stopped")))
        else
          coupleIfNeeded(after = after)
            .flatMap(after => /*`after` may have changed after initial AgentDedicated.*/
              getStreamX(after = after)
                .materialize.map(Checked.flattenTryChecked)
                .flatMap {
                  case Left(problem) =>
                    if isStopped then
                      IO.left(())  // Fail in next iteration
                    else if isSevereProblem(problem) then
                      IO.raiseError(problem.throwable)
                    else
                      (problem match {
                        case InvalidSessionTokenProblem =>
                          IO {
                            logger.debug(s"🔒 $api: $InvalidSessionTokenProblem")
                            true
                          }
                        case _ =>
                          onCouplingFailed(api, problem)
                      }).flatMap(continue =>
                        decouple *>
                          (if continue then
                            pauseBeforeRecoupling.as(Left(()))
                          else
                            IO.right(Stream.empty)))

                  case Right(stream) =>
                    IO.right(stream)
                }))

    private def getStreamX(after: I): IO[Checked[Stream[IO, V]]] =
      IO {
        sinceLastTry = now
      } *>
        getStream(api, after = after)
          //.timeout(idleTimeout)
          .onErrorRecoverWith:
            case t: TimeoutException =>
              logger.debug(s"💥 $api: ${t.toString}")
              IO.right(Stream.empty)

            case HttpException.HasProblem(problem) =>
              IO.left(problem)
          .map(_.map(obs =>
            idleTimeout.fold(obs)(idleTimeout => obs
              .timeoutOnSlowUpstream(idleTimeout)  // cancels upstream!
              .onErrorRecoverWith { case t: UpstreamTimeoutException =>
                logger.debug(s"💥 $api: ${t.toString}")
                // This should let Akka close the TCP connection to abort the stream
                Stream.empty
              })))

    private def coupleIfNeeded(after: I): IO[I] =
      coupledApiVar.tryRead.flatMap:
        case Some(_) => IO.pure(after)
        case None => tryEndlesslyToCouple(after)

    private def tryEndlesslyToCouple(after: I): IO[I] =
      logger.debugIO(IO.tailRecM(())(_ => IO.defer(
        if isStopped then
          IO.raiseError(new IllegalStateException(s"RecouplingStreamReader($api) has been stopped")
            with NoStackTrace)
        else
          ( for
              otherCoupledClient <- coupledApiVar.tryRead
              _ <- otherCoupledClient.fold(IO.unit)(_ =>
                IO.raiseError(new IllegalStateException("Coupling while already coupled")))
              _ <- IO { recouplingPause.onCouple() }
              _ <- api.login(onlyIfNotLoggedIn = true)//.timeout(idleTimeout)
              updatedIndex <- couple(index = after) /*AgentDedicated may return a different EventId*/
            yield updatedIndex
          ) .materializeIntoChecked
            .flatMap {
              case Left(problem) =>
                if isStopped then {
                  IO.left((()))
                } // Fail in next iteration
                else
                  for
                    _ <- IO.whenA(problem == InvalidSessionTokenProblem)(
                      api.tryLogout.void)
                    // TODO pekko.stream.scaladsl.TcpIdleTimeoutException sollte still ignoriert werden, ist aber abhängig von Pekko
                    continue <- onCouplingFailed(api, problem)
                    either <-
                      if continue then
                        pauseBeforeRecoupling.map(_ => Left(()))
                      else
                        IO.raiseError(problem.throwable)
                  yield either

              case Right(updatedIndex) =>
                for
                  _ <- coupledApiVar.put(api)
                  _ <- IO { recouplingPause.onCouplingSucceeded() }
                  _ <- onCoupled(api, after)
                yield Right(updatedIndex)
            })))

  private val pauseBeforeRecoupling =
    IO.defer(pauseBeforeNextTry(recouplingPause.nextPause()))


object RecouplingStreamReader:
  val TerminatedProblem = Problem.pure("RecouplingStreamReader has been stopped")

  private val PauseGranularity = 500.ms
  private val logger = Logger[this.type]

  def observe[@specialized(Long/*EventId or file position*/) I, V, Api <: SessionApi.HasUserAndPassword & HasIsIgnorableStackTrace](
    toIndex: V => I,
    api: Api,
    conf: RecouplingStreamReaderConf,
    after: I,
    getStream: I => IO[Checked[Stream[IO, V]]],
    eof: I => Boolean = (_: I) => false,
    stopRequested: () => Boolean = () => false)
  : Stream[IO, V]
  =
    val eof_ = eof
    val getStream_ = getStream
    val stopRequested_ = stopRequested
    new RecouplingStreamReader[I, V, Api](toIndex, conf) {
      def getStream(api: Api, after: I) = getStream_(after)
      override def eof(index: I) = eof_(index)
      def stopRequested = stopRequested_()
    }.observe(api, after)

  private def isSevereProblem(problem: Problem) =
    problem.is(EventSeqTornProblem) || problem.is(AckFromActiveClusterNodeProblem)

  private class RecouplingPause:
    // This class may be used asynchronously but not concurrently
    private val Minimum = 1.s
    @volatile private var pauses = initial
    @volatile private var lastCouplingTriedAt = now

    def onCouple() = lastCouplingTriedAt = now
    def onCouplingSucceeded() = pauses = initial
    def nextPause() = (lastCouplingTriedAt + synchronized { pauses.next() }).timeLeft max Minimum

    private def initial = Iterator(Minimum, 1.s, 1.s, 1.s, 2.s, 5.s) ++
      Iterator.continually(10.s)
