package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichPairTraversable
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.cast
import com.sos.scheduler.engine.minicom.idispatch.Invocable
import com.sos.scheduler.engine.taskserver.module.NamedInvocables
import com.sos.scheduler.engine.taskserver.module.NamedInvocables._

/**
 * @author Joacim Zschimmer
 */
final class TypedNamedInvocables private(val toMap: Map[String, Invocable]) extends NamedInvocables {
  lazy val spoolerLog: SpoolerLog = cast[SpoolerLog](toMap(SpoolerLogName))
  lazy val spoolerTask: SpoolerTask = cast[SpoolerTask](toMap(SpoolerTaskName))
  lazy val spoolerJob: Invocable = toMap(SpoolerJobName)
  lazy val spooler: Invocable = toMap(SpoolerName)

  def apply(name: String) = toMap(name)
}

object TypedNamedInvocables {
  def apply(kv: Iterable[(String, Invocable)]): TypedNamedInvocables = {
    val invalidNames = (kv map { _._1 }).toSet -- AllNames
    require(invalidNames.isEmpty, s"Invalid object names: $invalidNames")
    new TypedNamedInvocables(kv.uniqueToMap)
  }
}
