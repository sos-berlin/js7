package com.sos.jobscheduler.provider

import cats.syntax.flatMap._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.sos.jobscheduler.provider.Observing._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
private[provider] trait Observing {
  this: Provider ⇒

  private val minimumSilence    = conf.config.getDuration("jobscheduler.provider.file-watch.minimum-silence").toFiniteDuration
  private val watchDuration     = conf.config.getDuration("jobscheduler.provider.file-watch.poll-interval").toFiniteDuration
  private val errorWaitDuration = conf.config.getDuration("jobscheduler.provider.file-watch.error-delay").toFiniteDuration

  def observe(implicit s: Scheduler, iox: IOExecutor): Observable[Completed] = {
    // Start DirectoryWatcher before replaceMasterConfiguration, otherwise the first events may get lost!
    val directoryWatcher = new DirectoryWatcher(conf.liveDirectory, watchDuration)
    Observable.fromTask(
      retryUntilNoError(initialUpdateMasterConfiguration()))
      .appendAll(
        directoryWatcher.singleUseObservable
          .debounce(minimumSilence)
          .mapEval(_ ⇒
            retryUntilNoError(
              updateMasterConfiguration())))
  }

  private def retryUntilNoError[A](body: => Task[Checked[A]]): Task[A] =
    body.map(_.toTry).dematerialize  // Collapse Invalid and Failed
      .onErrorRestartLoop(()) { (throwable, _, retry) =>
        logger.error(s"Transfer failed: ${throwable.toStringWithCauses}")
        logger.debug(s"Transfer failed: ${throwable.toStringWithCauses}", throwable)
        api.logout().onErrorHandle(_ => ()) >>
          retry(()).delayExecution(errorWaitDuration)
      }
}

object Observing
{
  private val logger = Logger(getClass)
}
