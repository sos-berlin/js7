package js7.provider

import java.nio.file.Files.exists
import java.nio.file.Path
import js7.base.configutils.Configs.RichConfig
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.provider.Observing.*
import cats.effect.IO
import fs2.Stream

/**
  * @author Joacim Zschimmer
  */
private[provider] trait Observing extends OrderProvider:
  this: Provider =>

  private val minimumSilence    = conf.config.finiteDuration("js7.provider.directory-watch.minimum-silence").orThrow
  private val watchDuration     = conf.config.finiteDuration("js7.provider.directory-watch.poll-interval").orThrow
  private val errorWaitDuration = conf.config.finiteDuration("js7.provider.directory-watch.error-delay").orThrow

  final def observe: Stream[IO, Completed] =
    val streams = observeLive ::
      exists(conf.orderGeneratorsDirectory).thenList(observeOrderGenerators)
    Stream.combineLatestList(streams*)
      .takeUntilEval(untilStopRequested)
      .map((_: Seq[Completed]) => Completed)

  private def observeLive: Stream[IO, Completed] =
    observeDirectory(conf.liveDirectory, initiallyUpdateControllerConfiguration(), updateControllerConfiguration())

  private def observeOrderGenerators: Stream[IO, Completed] =
    startAddingOrders()  // No orders will be added before an OrderGenerator has been read from directory
    val replace = IO(replaceOrderGenerators.map(_ => Completed))
    observeDirectory(conf.orderGeneratorsDirectory, replace, replace)

  private def observeDirectory(
    directory: Path,
    replace: IO[Checked[Completed]],
    update: IO[Checked[Completed]])
  : Stream[IO, Completed] =
    // Start DirectoryWatcher before replaceControllerConfiguration, otherwise the first events may get lost!
    val directoryWatcher = new DirectoryWatcher(directory, watchDuration)
    Stream.fromIO(
      retryUntilNoError(replace))
      .appendAll(
        directoryWatcher.singleUseStream
          .guarantee(IO { directoryWatcher.close() })
          .debounce(minimumSilence)
          .mapEval(_ =>
            retryUntilNoError(update)))

  protected def retryUntilNoError[A](body: => IO[Checked[A]]): IO[A] =
    body
      .map(_.asTry).dematerialize  // Unify Success(Left(problem)) and Failure
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.warn(throwable.toStringWithCauses)
        throwable match
          case _: HttpClient.HttpException =>
          case _ => logger.debug(throwable.toStringWithCauses, throwable)
        val logout =
          if HttpClient.sessionMayBeLost(throwable) then
            httpControllerApi.logout().onErrorHandle { _ =>
              httpControllerApi.clearSession()
              Completed
            }
          else
            IO.unit
        logout >>
          loginUntilReachable.delayBy(errorWaitDuration) >>
          retry(())
      }


object Observing:
  private val logger = Logger[this.type]
