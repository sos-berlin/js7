package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class ProcessClassPath(string: String) extends TypedPath {

  if (string.nonEmpty) {  // ProcessClassPath.Default
    validate()
  }

  def companion = ProcessClassPath
}

object ProcessClassPath extends TypedPath.Companion[ProcessClassPath] {

  // 'def' due to mutual singleton dependency of this and FileBasedType
  def fileBasedType = FileBasedType.processClass

  val Default = ProcessClassPath("")
}
