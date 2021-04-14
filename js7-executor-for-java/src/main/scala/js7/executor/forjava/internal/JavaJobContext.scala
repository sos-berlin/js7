package js7.executor.forjava.internal

import js7.data.job.JobKey
import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import js7.executor.internal.InternalJob.JobContext
import scala.jdk.CollectionConverters._

trait JavaJobContext extends JavaWrapper
{
  type AsScala = JobContext

  def asScala: JobContext

  final val js7Executor: java.util.concurrent.Executor =
    asScala.js7Scheduler

  lazy val jobKey: JobKey =
    asScala.jobConf.jobKey

  lazy val jobArguments: java.util.Map[String, Value] =
    asScala.executable.jobArguments.asJava
}
