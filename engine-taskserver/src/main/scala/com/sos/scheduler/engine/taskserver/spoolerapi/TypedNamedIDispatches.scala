package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.base.utils.ScalaUtils.cast
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichPairTraversable
import com.sos.scheduler.engine.minicom.idispatch.IDispatch
import com.sos.scheduler.engine.taskserver.moduleapi.NamedIDispatches
import com.sos.scheduler.engine.taskserver.moduleapi.NamedIDispatches._

/**
 * @author Joacim Zschimmer
 */
final class TypedNamedIDispatches private(val toMap: Map[String, IDispatch]) extends NamedIDispatches {
  lazy val spoolerLog: SpoolerLog = cast[SpoolerLog](toMap(SpoolerLogName))
  lazy val spoolerTask: SpoolerTask = cast[SpoolerTask](toMap(SpoolerTaskName))
  lazy val spoolerJob: IDispatch = cast[IDispatch](toMap(SpoolerJobName))
  lazy val spooler: IDispatch = cast[IDispatch](toMap(SpoolerName))

  def apply(name: String) = toMap(name)
}

object TypedNamedIDispatches {
  def apply(kv: Iterable[(String, IDispatch)]): TypedNamedIDispatches = {
    val invalidNames = (kv map { _._1 }).toSet -- AllNames
    require(invalidNames.isEmpty, s"Invalid object names: $invalidNames")
    new TypedNamedIDispatches(kv.uniqueToMap)
  }
}
