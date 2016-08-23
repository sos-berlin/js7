package com.sos.scheduler.engine.data.filebased

/**
  * @author Joacim Zschimmer
  */
object FileBasedTypes {
  val forFiles: Set[FileBasedType] = (FileBasedType.values filter { _ ne FileBasedType.Folder }).toSet
}
