package com.sos.jobscheduler.provider

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.scalautil.IOExecutor
import com.sos.jobscheduler.common.time.ScalaTime.RichDuration
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.provider.Observing._
import com.sos.jobscheduler.provider.Provider.logThrowable
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
trait Observing {
  this: Provider ⇒

  private val minimumSilence    = conf.config.getDuration("jobscheduler.provider.file-watch.minimum-silence").toFiniteDuration
  private val watchDuration     = conf.config.getDuration("jobscheduler.provider.file-watch.poll-interval").toFiniteDuration
  private val errorWaitDuration = conf.config.getDuration("jobscheduler.provider.file-watch.error-delay").toFiniteDuration

  def observe()(implicit s: Scheduler, iox: IOExecutor): Observable[Unit] = {
    // Start DirectoryWatcher before replaceMasterConfiguration, otherwise the first events may get lost!
    val watcher = new DirectoryWatcher(conf.liveDirectory, watchDuration)
    val newVersionId = new VersionIdGenerator
    Observable.fromTask(replaceMasterConfiguration(newVersionId()).map(_ ⇒ ()))
      .appendAll(
        watcher.singleUseObservable
          .debounce(minimumSilence)
          .mapEval(_ ⇒
            updateMasterConfiguration(newVersionId())
            .map(_.toTry).dematerialize  // Collapse Invalid and Failed
            .materialize)
          .onErrorRecoverWith { case t ⇒
            Observable { logThrowable(t) } delayOnNext errorWaitDuration
          }
          .map(_ ⇒ ()))
  }
}

object Observing
{
  private class VersionIdGenerator {
    private val usedVersions = mutable.Set[VersionId]()  // TODO Grows endless. We need only the values of the last second (see VersionId.generate)

    def apply(): VersionId = {
      val v = VersionId.generate(usedVersions)
      usedVersions += v
      v
    }
  }
}
