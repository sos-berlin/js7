package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.filebased.TypedPath

final case class ProcessClassPath(string: String) extends TypedPath {

  if (string.nonEmpty) {  // ProcessClassPath.Default
    validate()
  }

  def companion = ProcessClassPath
}

object ProcessClassPath extends TypedPath.Companion[ProcessClassPath] {

  val Default = ProcessClassPath("")
}
