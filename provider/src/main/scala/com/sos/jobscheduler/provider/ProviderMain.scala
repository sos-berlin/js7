package com.sos.jobscheduler.provider

import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
object ProviderMain
{
  def main(args: Array[String]): Unit = {
    ProblemCodeMessages.initialize()
    val conf = ProviderConfiguration.fromCommandLine(args.toVector)
    new DirectoryToJobscheduler(conf).observe().foreach { _ ⇒ }.awaitInfinite
  }
}
