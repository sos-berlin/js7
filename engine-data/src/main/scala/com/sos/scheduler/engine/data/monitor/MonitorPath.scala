package com.sos.scheduler.engine.data.monitor

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

/**
 * @author Joacim Zschimmer
 */
final case class MonitorPath(string: String) extends TypedPath {
  requireIsAbsolute()

  def fileBasedType = FileBasedType.monitor
}

object MonitorPath extends TypedPath.Companion[MonitorPath]
