package com.sos.scheduler.engine.data.lock

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class LockPath(string: String) extends TypedPath {
  requireIsAbsolute()

  def fileBasedType = FileBasedType.lock
}

object LockPath extends TypedPath.Companion[LockPath]
