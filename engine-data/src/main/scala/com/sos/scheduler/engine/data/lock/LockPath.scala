package com.sos.scheduler.engine.data.lock

import com.sos.scheduler.engine.data.filebased.TypedPath

final case class LockPath(string: String) extends TypedPath {
  validate()

  def companion = LockPath
}

object LockPath extends TypedPath.Companion[LockPath]
