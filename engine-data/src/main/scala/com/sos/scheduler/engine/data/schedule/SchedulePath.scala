package com.sos.scheduler.engine.data.schedule

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class SchedulePath(string: String) extends TypedPath {
  requireIsAbsolute()

  def fileBasedType = FileBasedType.schedule
}
