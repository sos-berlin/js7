package js7.launcher.forjava.internal

import java.nio.charset.Charset
import js7.data.job.JobKey
import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import js7.launcher.internal.InternalJob.JobContext
import scala.jdk.CollectionConverters.*

trait JavaJobContext extends JavaWrapper:
  type AsScala = JobContext

  def asScala: JobContext

  lazy val jobKey: JobKey =
    asScala.jobConf.jobKey

  // Scala 3.6.2 erases type parameters from lazy val. So we publish via a def-function.
  private lazy val _jobArguments: java.util.Map[String, Value] =
    asScala.jobArguments.asJava

  def jobArguments: java.util.Map[String, Value] =
    _jobArguments

  def systemEncoding: Charset =
    asScala.systemEncoding

  /** The `Executor` internally used by JS7.
    * <ul><li>
    * Use only if you know what you to!
    * <li>
    * Never provide a thread-blocking `Runnable`!
    * </li>
    */
  final val internalJs7Executor: java.util.concurrent.Executor =
    asScala.ioRuntime.compute.execute(_)
