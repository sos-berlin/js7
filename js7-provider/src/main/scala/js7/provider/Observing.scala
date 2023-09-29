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
import monix.eval.Task
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
private[provider] trait Observing extends OrderProvider:
  this: Provider =>

  private val minimumSilence    = conf.config.finiteDuration("js7.provider.directory-watch.minimum-silence").orThrow
  private val watchDuration     = conf.config.finiteDuration("js7.provider.directory-watch.poll-interval").orThrow
  private val errorWaitDuration = conf.config.finiteDuration("js7.provider.directory-watch.error-delay").orThrow

  final def observe: Observable[Completed] =
    val observables = observeLive ::
      exists(conf.orderGeneratorsDirectory).thenList(observeOrderGenerators)
    Observable.combineLatestList(observables*)
      .takeUntilEval(untilStopRequested)
      .map((_: Seq[Completed]) => Completed)

  private def observeLive: Observable[Completed] =
    observeDirectory(conf.liveDirectory, initiallyUpdateControllerConfiguration(), updateControllerConfiguration())

  private def observeOrderGenerators: Observable[Completed] =
    startAddingOrders()  // No orders will be added before an OrderGenerator has been read from directory
    val replace = Task(replaceOrderGenerators.map(_ => Completed))
    observeDirectory(conf.orderGeneratorsDirectory, replace, replace)

  private def observeDirectory(
    directory: Path,
    replace: Task[Checked[Completed]],
    update: Task[Checked[Completed]])
  : Observable[Completed] =
    // Start DirectoryWatcher before replaceControllerConfiguration, otherwise the first events may get lost!
    val directoryWatcher = new DirectoryWatcher(directory, watchDuration)
    Observable.fromTask(
      retryUntilNoError(replace))
      .appendAll(
        directoryWatcher.singleUseObservable
          .guarantee(Task { directoryWatcher.close() })
          .debounce(minimumSilence)
          .mapEval(_ =>
            retryUntilNoError(update)))

  protected def retryUntilNoError[A](body: => Task[Checked[A]]): Task[A] =
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
            Task.unit
        logout >>
          loginUntilReachable.delayExecution(errorWaitDuration) >>
          retry(())
      }

object Observing:
  private val logger = Logger[this.type]
