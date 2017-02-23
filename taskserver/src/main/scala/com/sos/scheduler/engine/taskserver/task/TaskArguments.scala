package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions.ImplicitConvertablePF
import com.sos.scheduler.engine.base.utils.ScalaUtils._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.xml.VariableSets
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.types.{VariantArray, variant}
import com.sos.scheduler.engine.taskserver.moduleapi._
import com.sos.scheduler.engine.taskserver.modules.monitor.{Monitor, RawMonitorArguments}
import com.sos.scheduler.engine.taskserver.task.TaskArguments._
import java.nio.file.Paths
import scala.collection.{immutable, mutable}
import scala.util.Sorting.stableSort

/**
 * @author Joacim Zschimmer
 */
final class TaskArguments private(arguments: List[(String, String)]) {

  lazy val environment: Map[String, String] = VariableSets.parseXml(apply(EnvironmentKey))
  lazy val hasOrder = get(HasOrderKey) match {
    case Some("1") ⇒ true
    case Some(o) ⇒ throw new IllegalArgumentException(s"Invalid agent argument: $HasOrderKey=$o")
    case None ⇒ false
  }
  lazy val rawModuleArguments = extractRawModuleArguments(arguments)
  lazy val jobName: String = apply(JobKey)
  lazy val shellVariablePrefix: String = get(ShellVariablePrefixKey) getOrElse DefaultShellVariablePrefix
  lazy val stderrLogLevel: SchedulerLogLevel = get(StderrLogLevelKey) map { o ⇒ SchedulerLogLevel.ofCpp(o.toInt) } getOrElse SchedulerLogLevel.info
  lazy val taskId: TaskId = TaskId(apply(TaskIdKey).toInt)
  lazy val rawMonitorArguments: immutable.Seq[RawMonitorArguments] = {
    val unordered =
      for (m ← splitMonitorArguments(arguments collect { case (k, v) if k startsWith MonitorPrefix ⇒ (k stripPrefix MonitorPrefix) → v }))
      yield RawMonitorArguments(m.rawModuleArguments, name = m.name, ordering = m.ordering)
    stableSort(unordered, { o: RawMonitorArguments ⇒ o.ordering }).toVector
  }

  private def apply(name: String) = get(name) getOrElse { throw new NoSuchElementException(s"Agent argument '$name' not given") }

  private def get(name: String): Option[String] = arguments collectFirst { case (k, v) if k == name ⇒ v }

  private def splitMonitorArguments(args: List[(String, String)]): List[MonitorArguments] =
    // For every monitor definition, "monitor.script" is the last argument
    args indexWhere { _._1 == module.ScriptKey } match {
      case -1 ⇒ Nil
      case scriptIndex ⇒
        val (head, tail) = args splitAt scriptIndex + 1
        new MonitorArguments(head.toMap) :: splitMonitorArguments(tail)
    }

  private class MonitorArguments(argMap: Map[String, String]) {
    def name = argMap.getOrElse(monitor.NameKey, "")
    val ordering = argMap.as[Int](monitor.OrderingKey, Monitor.DefaultOrdering)
    val rawModuleArguments = extractRawModuleArguments(argMap)
  }

  private def extractRawModuleArguments(args: Iterable[(String, String)]) = {
    val argMap = args.toMap
    val javaClassNameOption = argMap.get(module.JavaClassKey) filter { _.nonEmpty }
    RawModuleArguments(
      language = ModuleLanguage(argMap(module.LanguageKey).toLowerCase),
      javaClassNameOption = javaClassNameOption,
      dotnetClassNameOption = argMap.get(module.DotnetClassKey) filter { _.nonEmpty },
      dllOption = argMap.get(module.DllKey) filter { _.nonEmpty } map { o ⇒ Paths.get(o) },
      script = argMap.getOrElse(module.ScriptKey, "") match {
        case "" ⇒ new Script("")
        case string ⇒ Script.parseXmlString(string)
      })
  }
}

object TaskArguments {
  val DefaultShellVariablePrefix = "SCHEDULER_PARAM_"
  val EnvironmentKey = "environment"
  val HasOrderKey = "has_order"
  val JobKey = "job"
  val ShellVariablePrefixKey = "process.shell_variable_prefix"
  val StderrLogLevelKey = "stderr_log_level"
  val TaskIdKey = "task_id"
  object module {
    val LanguageKey = "language"
    val ScriptKey = "script"
    val JavaClassKey = "java_class"
    val DotnetClassKey = "dotnet_class"
    val DllKey = "dll"
    val KeySet = Set(LanguageKey, ScriptKey, JavaClassKey, DotnetClassKey, DllKey)
  }
  private object monitor {
    val NameKey = "name"
    val OrderingKey = "ordering"
    val KeySet = module.KeySet ++ Set(NameKey, OrderingKey)
  }
  private val MonitorPrefix = "monitor."
  private val KeySet = Set(EnvironmentKey, HasOrderKey, JobKey,
    ShellVariablePrefixKey, StderrLogLevelKey, TaskIdKey) ++
    module.KeySet ++
    (monitor.KeySet map { o ⇒ s"$MonitorPrefix$o" })
  private val IsLegacyKeyValue = Set(
    "com_class" → "",
    "filename" → "",
    "java_class_path" → "",
    "java_options" → "",
    "monitor.com_class" → "",
    "monitor.filename" → "",
    "process.filename" → "",
    "process.log_filename" → "",
    "process.ignore_error" → "0",
    "process.ignore_signal" → "0",
    "process.param_raw" → "")
  assert(((IsLegacyKeyValue map { _._1 }) intersect KeySet).isEmpty)
  private val KeyValueRegex = "(?s)([[a-z_.]]+)=(.*)".r  //  "(?s)" dot matches \n too, "key=value"
  private val logger = Logger(getClass)

  def apply(arguments: VariantArray): TaskArguments =
    apply(arguments.indexedSeq filterNot variant.isEmpty map cast[String])

  def apply(keyValues: immutable.Seq[String]): TaskArguments = {
    val buffer = mutable.Buffer[(String, String)]()
    for (keyValueString ← keyValues) {
      val KeyValueRegex(key, value) = keyValueString
      if (KeySet contains key) {
        buffer += key → value
      } else
      if (!IsLegacyKeyValue(key → value.trim)) {
        logger.warn(s"Ignoring unsupported key: $key=$value")
      }
    }
    new TaskArguments(buffer.toList)
  }

  def toStrings(keyValues: Iterable[(String, String)]): immutable.Seq[String] =
    (keyValues map { case (k, v) ⇒ s"$k=$v" }).toImmutableSeq
}
