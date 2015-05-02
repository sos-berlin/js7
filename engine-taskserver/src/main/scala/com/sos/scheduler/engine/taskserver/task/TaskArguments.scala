package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.ScalaUtils._
import com.sos.scheduler.engine.common.xml.VariableSets
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.minicom.types.{VariantArray, variant}
import com.sos.scheduler.engine.taskserver.module._
import com.sos.scheduler.engine.taskserver.task.TaskArguments._
import scala.collection.{immutable, mutable}
import scala.util.Sorting.stableSort

/**
 * @author Joacim Zschimmer
 */
private[task] final class TaskArguments private(arguments: List[(String, String)]) {

  lazy val moduleLanguage: ModuleLanguage = ModuleLanguage(apply(LanguageKey))
  lazy val script: Script = Script.parseXmlString(apply(ScriptKey))
  lazy val jobName: String = apply(JobKey)
  lazy val taskId: TaskId = TaskId(apply(TaskIdKey).toInt)
  lazy val environment: Map[String, String] = VariableSets.parseXml(apply(EnvironmentKey))
  lazy val javaClassNameOption: Option[String] = get(JavaClassKey) filter { _.nonEmpty }

  lazy val hasOrder = get(HasOrderKey) match {
    case Some("1") ⇒ true
    case Some(o) ⇒ throw new IllegalArgumentException(s"Invalid agent argument: $HasOrderKey=$o")
    case None ⇒ false
  }

  lazy val module = Module(moduleLanguage, script, javaClassNameOption)

  lazy val monitors: immutable.Seq[Monitor] = {
    val unordered =
      for (m ← splitMonitorArguments(arguments filter { _._1 startsWith "monitor." })) yield {
        val module = Module(m.moduleLanguage, m.script, m.javaClassNameOption)
        Monitor(module, name = m.name, ordering = m.ordering)
      }
    stableSort(unordered, { o: Monitor ⇒ o.ordering }).toImmutableSeq
  }

  private def apply(name: String) = get(name) getOrElse { throw new NoSuchElementException(s"Agent argument '$name' not given") }

  private def get(name: String): Option[String] = arguments collectFirst { case (k, v) if k == name ⇒ v }
}

private[task] object TaskArguments {
  private val LanguageKey = "language"
  private val JavaClassKey = "java_class"
  private val ScriptKey = "script"
  private val JobKey = "job"
  private val TaskIdKey = "task_id"
  private val EnvironmentKey = "environment"
  private val HasOrderKey = "has_order"
  //TODO private val ProcessShellVariablePrefixKey = "process.shell_variable_prefix"
  private val MonitorLanguageKey = "monitor.language"
  private val MonitorNameKey = "monitor.name"
  private val MonitorOrderingKey = "monitor.ordering"
  private val MonitorJavaClassKey = "monitor.java_class"
  private val MonitorScriptKey = "monitor.script"
  private val KeySet = Set(LanguageKey, ScriptKey, JobKey, TaskIdKey, EnvironmentKey, HasOrderKey, JavaClassKey,
    MonitorLanguageKey, MonitorNameKey, MonitorOrderingKey, MonitorJavaClassKey, MonitorScriptKey)
  private val KeyValueRegex = "(?s)([[a-z_.]]+)=(.*)".r  //  "(?s)" dot matches \n too, "key=value"
  private val logger = Logger(getClass)

  def apply(arguments: VariantArray): TaskArguments = {
    val buffer = mutable.Buffer[(String, String)]()
    for (keyValueString ← arguments.indexedSeq filterNot variant.isEmpty map cast[String]) {
      val KeyValueRegex(key, value) = keyValueString
      if (KeySet contains key) {
        buffer += key → value
      } else {
        logger.debug(s"Ignoring unsupported key: $key=$value")
      }
    }
    new TaskArguments(buffer.toList)
  }

  private def splitMonitorArguments(args: List[(String, String)]): List[MonitorArguments] =
    // For every monitor definition, "monitor.script" is the last argument
    args indexWhere { _._1 == MonitorScriptKey } match {
      case -1 ⇒ Nil
      case scriptIndex ⇒
        val (head, tail) = args splitAt scriptIndex + 1
        new MonitorArguments(head.toMap) :: splitMonitorArguments(tail)
    }

  private class MonitorArguments(argMap: Map[String, String]) {
    def moduleLanguage = ModuleLanguage(argMap(MonitorLanguageKey))
    def name = argMap.getOrElse(MonitorNameKey, "")
    def ordering = argMap.getConverted(MonitorOrderingKey) { _.toInt } getOrElse Monitor.DefaultOrdering
    def javaClassNameOption = argMap.get(MonitorJavaClassKey)
    def script = javaClassNameOption match {
      case None | Some("") ⇒ Script.parseXmlString(argMap(MonitorScriptKey))
      case Some(o) ⇒ new Script("")
    }
  }
}
