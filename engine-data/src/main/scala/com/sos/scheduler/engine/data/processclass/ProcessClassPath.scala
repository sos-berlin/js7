package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}

final case class ProcessClassPath(string: String) extends TypedPath {
  if (!isEmpty) requireIsAbsolute()   // There is the default process class named ""

  def fileBasedType = FileBasedType.processClass
}

object ProcessClassPath extends TypedPath.Companion[ProcessClassPath] {
  def Default = ProcessClassPath("")
}
