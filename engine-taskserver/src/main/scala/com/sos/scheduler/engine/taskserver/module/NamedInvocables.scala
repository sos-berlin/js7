package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichPairTraversable
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.minicom.idispatch.Invocable
import com.sos.scheduler.engine.taskserver.module.NamedInvocables._
import com.sos.scheduler.engine.taskserver.spoolerapi.{SpoolerLog, SpoolerTask}

/**
 * @author Joacim Zschimmer
 */
final class NamedInvocables private(val toMap: Map[String, Invocable]) {
  lazy val spoolerLog: SpoolerLog = cast[SpoolerLog](toMap(SpoolerLogName))
  lazy val spoolerTask: SpoolerTask = cast[SpoolerTask](toMap(SpoolerTaskName))
  lazy val spoolerJob: Invocable = toMap(SpoolerJobName)
  lazy val spooler: Invocable = toMap(SpoolerName)

  def apply(name: String) = toMap(name)
}

object NamedInvocables {
  val SpoolerLogName = "spooler_log"
  val SpoolerTaskName = "spooler_task"
  val SpoolerJobName = "spooler_job"
  val SpoolerName = "spooler"
  val AllNames = Set(SpoolerLogName, SpoolerTaskName, SpoolerJobName, SpoolerName)

  def apply(kv: Iterable[(String, Invocable)]): NamedInvocables = {
    val invalidNames = (kv map { _._1 }).toSet -- AllNames
    require(invalidNames.isEmpty, s"Invalid object names: $invalidNames")
    new NamedInvocables(kv.uniqueToMap)
  }
}
