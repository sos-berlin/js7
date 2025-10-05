package js7.provider

import cats.effect.IO
import fs2.Stream
import java.nio.file.Files.exists
import java.nio.file.Path
import js7.base.configutils.Configs.RichConfig
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.{dematerialize, onErrorRestartLoop}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.provider.Observing.*

/**
  * @author Joacim Zschimmer
  */
private[provider] trait Observing:
  this: Provider =>

  private val minimumSilence    = conf.config.finiteDuration("js7.provider.directory-watch.minimum-silence").orThrow
  private val watchDuration     = conf.config.finiteDuration("js7.provider.directory-watch.poll-interval").orThrow
  private val errorWaitDuration = conf.config.finiteDuration("js7.provider.directory-watch.error-delay").orThrow

  final def stream: Stream[IO, Unit] =
    Stream
      .iterable:
        liveStream :: exists(conf.orderGeneratorsDirectory).thenList(orderGeneratorStream)
      .parJoinUnbounded
      .interruptWhen(untilStopRequested.attempt)

  private def liveStream: Stream[IO, Unit] =
    observeDirectory(conf.liveDirectory, initiallyUpdateControllerConfiguration(), updateControllerConfiguration())

  private def orderGeneratorStream: Stream[IO, Unit] =
    Stream.resource:
      OrderProvider.service(httpControllerApi, retryUntilNoError, conf)
      // No orders will be added before an OrderGenerator has been read from directory
    .flatMap: orderProvider =>
      val replace = IO(orderProvider.replaceOrderGenerators)
      observeDirectory(conf.orderGeneratorsDirectory, replace, replace)

  private def observeDirectory(
    directory: Path,
    replace: IO[Checked[Unit]],
    update: IO[Checked[Unit]])
  : Stream[IO, Unit] =
    // Start DirectoryWatcher before replaceControllerConfiguration, otherwise the first events may get lost!
    val directoryWatcher = new DirectoryWatcher(directory, watchDuration)
    Stream
      .eval:
        retryUntilNoError(replace)
      .append:
        directoryWatcher.singleUseStream
          .onFinalize(IO { directoryWatcher.close() })
          .debounce(minimumSilence)
          .evalMap(_ =>
            retryUntilNoError(update))

  protected def retryUntilNoError(body: => IO[Checked[Unit]]): IO[Unit] =
    body
      .map(_.asTry).dematerialize  // Unify Success(Left(problem)) and Failure
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.warn(throwable.toStringWithCauses)
        throwable match
          case _: HttpClient.HttpException =>
          case _ => logger.debug(throwable.toStringWithCauses, throwable)
        val logout =
          if HttpClient.sessionMayBeLost(throwable) then
            httpControllerApi.logout().handleError: _ =>
              httpControllerApi.clearSession()
              Completed
          else
            IO.unit
        logout >>
          loginUntilReachable.delayBy(errorWaitDuration) >>
          retry(())
      }


object Observing:
  private val logger = Logger[this.type]
