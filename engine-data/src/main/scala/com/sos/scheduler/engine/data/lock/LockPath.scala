package com.sos.scheduler.engine.data.lock

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class LockPath(string: String) extends TypedPath {
  validate()

  def companion = LockPath
}

object LockPath extends TypedPath.Companion[LockPath] {

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.Lock
}
