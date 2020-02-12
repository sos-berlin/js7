package com.sos.jobscheduler.provider

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.provider.Observing._
import java.nio.file.Files.exists
import java.nio.file.Path
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
private[provider] trait Observing extends OrderProvider {
  this: Provider =>

  private val minimumSilence    = conf.config.getDuration("jobscheduler.provider.directory-watch.minimum-silence").toFiniteDuration
  private val watchDuration     = conf.config.getDuration("jobscheduler.provider.directory-watch.poll-interval").toFiniteDuration
  private val errorWaitDuration = conf.config.getDuration("jobscheduler.provider.directory-watch.error-delay").toFiniteDuration

  def observe(implicit s: Scheduler, iox: IOExecutor): Observable[Completed] = {
    val observables = observeLive ::
      exists(conf.orderGeneratorsDirectory).thenList(observeOrderGenerators)
    Observable.combineLatestList(observables: _*)
      .map((_: Seq[Completed]) => Completed)
  }

  private def observeLive(implicit s: Scheduler, iox: IOExecutor): Observable[Completed] =
    observeDirectory(conf.liveDirectory, initiallyUpdateMasterConfiguration(), updateMasterConfiguration())

  private def observeOrderGenerators(implicit s: Scheduler, iox: IOExecutor): Observable[Completed] = {
    startAddingOrders()  // No orders will be added before an OrderGenerator has been read from directory
    val replace = Task(replaceOrderGenerators.map(_ => Completed))
    observeDirectory(conf.orderGeneratorsDirectory, replace, replace)
  }

  private def observeDirectory(directory: Path, replace: Task[Checked[Completed]], update: Task[Checked[Completed]])
    (implicit s: Scheduler, iox: IOExecutor)
  : Observable[Completed] = {
    // Start DirectoryWatcher before replaceMasterConfiguration, otherwise the first events may get lost!
    val directoryWatcher = new DirectoryWatcher(directory, watchDuration)
    Observable.fromTask(
      retryUntilNoError(replace))
      .appendAll(
        directoryWatcher.singleUseObservable
          .guarantee(Task { directoryWatcher.close() })
          .debounce(minimumSilence)
          .mapEval(_ =>
            retryUntilNoError(update)))
  }

  protected def retryUntilNoError[A](body: => Task[Checked[A]]): Task[A] =
    body
      .map(_.asTry).dematerialize  // Unify Success(Left(problem)) and Failure
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.error(throwable.toStringWithCauses)
        throwable match {
          case _: AkkaHttpClient.HttpException =>
          case _ => logger.debug(throwable.toStringWithCauses, throwable)
        }
        (relogin >> retry(()))
          .delayExecution(errorWaitDuration)
      }
}

object Observing
{
  private val logger = Logger(getClass)
}
