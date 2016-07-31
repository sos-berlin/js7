package com.sos.scheduler.engine.data.monitor

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

/**
 * @author Joacim Zschimmer
 */
final case class MonitorPath(string: String) extends TypedPath {
  validate()

  def companion = MonitorPath
}

object MonitorPath extends TypedPath.Companion[MonitorPath] {

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.monitor
}
