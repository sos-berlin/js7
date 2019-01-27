package com.sos.jobscheduler.provider

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.scalautil.IOExecutor
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.sos.jobscheduler.common.utils.CatsUtils.autoCloseableToResource
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.provider.Provider.logThrowable
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import monix.execution.Scheduler
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final class DirectoryToJobscheduler(conf: ProviderConfiguration)
{
  private val minimumSilence    = conf.config.getDuration("jobscheduler.provider.file-watch.minimum-silence").toFiniteDuration
  private val watchDuration     = conf.config.getDuration("jobscheduler.provider.file-watch.poll-interval").toFiniteDuration
  private val errorWaitDuration = conf.config.getDuration("jobscheduler.provider.file-watch.error-delay").toFiniteDuration

  def observe()(implicit s: Scheduler, iox: IOExecutor): Observable[Unit] =
    autoCloseableToResource(new Provider(conf)).flatMap(provider ⇒
      DirectoryWatcher.observe(conf.liveDirectory, watchDuration)
        .debounce(minimumSilence)
        .mapEval(_ ⇒ provider.updateMaster()
          .map(_.toTry).dematerialize  // Collapse Invalid and Failed
          .map((_: MasterCommand.Response.Accepted) ⇒ ())
          .materialize)
        .onErrorRecoverWith { case t ⇒
          Observable { logThrowable(t) } delayOnNext errorWaitDuration
        }
        .map(_ ⇒ ()))
}
