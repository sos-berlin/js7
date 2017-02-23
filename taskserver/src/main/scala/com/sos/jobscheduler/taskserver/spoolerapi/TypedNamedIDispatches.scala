package com.sos.jobscheduler.taskserver.spoolerapi

import com.sos.jobscheduler.base.utils.ScalaUtils.cast
import com.sos.jobscheduler.common.scalautil.Collections.implicits.RichPairTraversable
import com.sos.jobscheduler.minicom.idispatch.IDispatch
import com.sos.jobscheduler.taskserver.moduleapi.NamedIDispatches
import com.sos.jobscheduler.taskserver.moduleapi.NamedIDispatches._

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
