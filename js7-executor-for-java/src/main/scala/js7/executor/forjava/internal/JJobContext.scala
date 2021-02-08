package js7.executor.forjava.internal

import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import js7.executor.internal.InternalJob.JobContext
import scala.jdk.CollectionConverters._

final case class JJobContext(
  asScala: JobContext,
  js7Executor: java.util.concurrent.Executor)
extends JavaWrapper
{
  type AsScala = JobContext

  lazy val jobArguments: java.util.Map[String, Value] =
    asScala.jobArguments.asJava
}
