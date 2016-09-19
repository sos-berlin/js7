package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.FileBasedType.{Folder, Unknown}

/**
  * @author Joacim Zschimmer
  */
object FileBasedTypes {
  val forFiles: Set[FileBasedType] = (FileBasedType.values filter { o ⇒ (o ne Folder) && (o ne Unknown) }).toSet
}
