package js7.common.http

import cats.effect.{Deferred, IO}
import cats.syntax.flatMap.*
import fs2.Stream
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.fs2utils.StreamExtensions.{+:, interruptWhenF}
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixlike.MonixLikeExtensions.materialize
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests
import js7.base.utils.Tests.isTest
import js7.common.http.RecouplingStreamReader.*
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.Problems.AckFromActiveClusterNodeProblem
import js7.data.event.EventSeqTornProblem
import js7.data.problems.UnknownEventIdProblem
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.util.control.NoStackTrace

/** Logs in, couples and fetches objects from a (HTTP) stream, and recouples after error. */
abstract class RecouplingStreamReader[
  @specialized(Long/*EventId or file position*/) I,
  V: Tag,
  Api <: SessionApi.HasUserAndPassword & HasIsIgnorableStackTrace,
](toIndex: V => Option[I],
  conf: RecouplingStreamReaderConf):

  private val sym = new BlockingSymbol

  protected def couple(index: I): IO[Checked[I]] =
    IO.right(index)

  protected def getStream(api: Api, after: I): IO[Checked[Stream[IO, V]]]

  protected def onCouplingFailed(api: Api, problem: Problem): IO[Boolean] =
    inUse.get
      .flatMap(inUse =>
        IO {
          var logged = false
          lazy val msg = s"$api: coupling failed: $problem"
          if inUse && !stopRequested && !coupledApiVar.isStopped then
            sym.onWarn()
            logger.warn(s"$sym $msg")
            logged = true
          for throwable <- problem.throwableOption.map(_.nullIfNoStackTrace)
               if api.hasRelevantStackTrace(throwable) do {
            logger.debug(s"💥 $msg", throwable)
            logged = true
          }
          if !logged then
            logger.debug(s"💥 $api: $msg")
          true  // Recouple and continue
        })

  protected def onCoupled(api: Api, after: I): IO[Completed] =
    IO.completed

  protected def onDecoupled: IO[Completed] =
    IO.completed

  protected def eof(index: I) = false

  // TODO Genügt nicht `terminate` ?
  protected def stopRequested: Boolean

  protected def idleTimeout: Option[FiniteDuration] =
    conf.timeout
    //??? requestTimeout.map(_ + 2.s)/*let service timeout kick in first*/

  private def isStopped =
    stopRequested || coupledApiVar.isStopped || !inUse.is

  private val stopped = Deferred.unsafe[IO, Unit]
  private val coupledApiVar = new CoupledApiVar[Api]
  private val recouplingPause = new RecouplingPause
  private val inUse = new InUse
  private var sinceLastTry = now - 1.hour

  /** Observes endlessly, recoupling and repeating when needed. */
  final def stream(api: Api, after: I): Stream[IO, V] =
    logger.debugStream("stream", s"$api after=$after"):
      Stream
        .resource(inUse.resource(api))
        .evalTap: _ =>
          decouple
        .flatMap: _ =>
          ForApi(api).streamAgainAndAgain(after)
        .interruptWhenF(stopped.get)

  final def terminateAndLogout: IO[Unit] =
    logger.traceIO:
      stopStreaming
        .*>(coupledApiVar.terminate)
        .logWhenItTakesLonger

  def stopStreaming: IO[Unit] =
    logger.traceIO:
      stopped.complete(()).void

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
    IO.defer:
      IO.sleep:
        (sinceLastTry + delay).timeLeftOrZero.roundUpToNext(PauseGranularity)
      .map: _ =>
        sinceLastTry = now  // update asynchronously

  private final class ForApi(api: Api):

    def streamAgainAndAgain(after: I): Stream[IO, V] =
      logger.traceStream:
        var lastIndex = after
        after
          .tailRecM: after =>
            if eof(after) || isStopped then
              Stream.emit(Right(Stream.empty))
            else
              Right:
                Stream.eval:
                  streamOnceAfter(after)
                .flatMap: (i, stream) =>
                  lastIndex = i
                  stream
                    .map: v =>
                      for i <- toIndex(v) do
                        lastIndex = i
                      v
                    .handleErrorWith:
                      case t: ProblemException if isSevereProblem(t.problem) =>
                        Stream.raiseError[IO](t)
                      case t =>
                        Stream
                          .eval:
                            onCouplingFailed(api, Problem.fromThrowable(t))
                          .flatMap:
                            case false => Stream.raiseError[IO](t)
                            case true => Stream.empty
              +:
                Stream.eval:
                  pauseBeforeNextTry(conf.delay) *>
                    IO.defer(IO.left(lastIndex))
          .flatten

    private def streamOnceAfter(after: I): IO[(I, Stream[IO, V])] =
      tryEndlesslyToGetStream(after) <*
        IO:
          logger.log(sym.relievedLogLevel, s"🟢 Streaming $api ...")
          sym.clear()

    /** Retries until web request returns an Stream. */
    private def tryEndlesslyToGetStream(after: I): IO[(I, Stream[IO, V])] =
      logger.traceIO:
        ().tailRecM: _ =>
          if isStopped then
            IO.right(after -> Stream.empty)
            //IO.right(Stream.raiseError[IO](
            //  new IllegalStateException(s"RecouplingStreamReader($api) has been stopped")))
          else
            coupleIfNeeded(after = after)
              .flatMap: after => /*`after` may have changed after initial AgentDedicated.*/
                getStreamX(after = after)
                  .materialize.map(Checked.flattenTryChecked)
                  .flatMap:
                    case Left(problem) =>
                      if isStopped then
                        IO.left(())  // Fail in next iteration
                      else if isSevereProblem(problem) then
                        IO.raiseError(problem.throwable)
                      else
                        onCouplingFailed(api, problem).flatMap: continue =>
                          decouple *>
                            (if continue then
                              pauseBeforeRecoupling.as(Left(()))
                            else
                              IO.right(after -> Stream.empty))

                    case Right(stream) =>
                      IO.right(after -> stream)

    private def getStreamX(after: I): IO[Checked[Stream[IO, V]]] =
      logger.traceIO("getStreamX", s"after=$after"):
        IO.defer:
          sinceLastTry = now
          getStream(api, after = after)

    private def coupleIfNeeded(after: I): IO[I] =
      coupledApiVar.tryRead.flatMap:
        case Some(_) => IO.pure(after)
        case None => tryEndlesslyToCouple(after)

    private def tryEndlesslyToCouple(after: I): IO[I] =
      logger.debugIO(().tailRecM(_ => IO.defer(
        if isStopped then
          IO.raiseError(new IllegalStateException(s"RecouplingStreamReader($api) has been stopped")
            with NoStackTrace)
        else
          ( for
              otherCoupledClient <- coupledApiVar.tryRead
              _ <- otherCoupledClient.fold(IO.unit): _ =>
                IO.raiseError(new IllegalStateException("Coupling while already coupled"))
              _ <- IO { recouplingPause.onCouple() }
              _ <- api.login(onlyIfNotLoggedIn = true)//.timeout(idleTimeout)
              updatedIndex <- couple(index = after) /*AgentDedicated may return a different EventId*/
            yield updatedIndex
          ) .materializeIntoChecked
            .flatMap {
              case Left(problem) =>
                if isStopped then
                  IO.left(())
                else
                  // Fail in next iteration
                  for
                    // ??? pekko.stream.scaladsl.TcpIdleTimeoutException sollte still ignoriert werden, ist aber abhängig von Pekko
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
  val TerminatedProblem: Problem = Problem.pure("RecouplingStreamReader has been stopped")

  private val PauseGranularity = 500.ms
  private val logger = Logger[this.type]

  def stream[
    @specialized(Long/*EventId or file position*/) I,
    V: Tag,
    Api <: SessionApi.HasUserAndPassword & HasIsIgnorableStackTrace
  ](toIndex: V => Option[I],
    api: Api,
    conf: RecouplingStreamReaderConf,
    after: I,
    getStream: I => IO[Checked[Stream[IO, V]]],
    eof: I => Boolean = (_: I) => false,
    stopRequested: () => Boolean = () => false)
  : Stream[IO, V] =
    val eof_ = eof
    val getStream_ = getStream
    val stopRequested_ = stopRequested
    new RecouplingStreamReader[I, V, Api](toIndex, conf) {
      def getStream(api: Api, after: I) = getStream_(after)
      override def eof(index: I) = eof_(index)
      def stopRequested = stopRequested_()
    }.stream(api, after)

  private def isSevereProblem(problem: Problem) =
    problem.is(UnknownEventIdProblem)
    || problem.is(EventSeqTornProblem)
    || problem.is(AckFromActiveClusterNodeProblem)

  private class RecouplingPause:
    // This class may be used asynchronously but not concurrently
    private val Minimum = 1.s
    @volatile private var pauses = initial
    @volatile private var lastCouplingTriedAt = now

    def onCouple(): Unit =
      lastCouplingTriedAt = now

    def onCouplingSucceeded(): Unit =
      pauses = initial

    def nextPause() =
      (lastCouplingTriedAt + synchronized(pauses.next())).timeLeft max Minimum

    private def initial = Iterator(Minimum, 1.s, 1.s, 1.s, 2.s, 5.s) ++
      Iterator.continually(10.s)
