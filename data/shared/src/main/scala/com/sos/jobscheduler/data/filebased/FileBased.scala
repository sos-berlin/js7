package com.sos.jobscheduler.data.filebased

/**
  * @author Joacim Zschimmer
  */
trait FileBased {
  type ThisTypedPath <: TypedPath

  def path: TypedPath
}

object FileBased {
  trait Companion {
    type ThisFileBased <: FileBased
    type ThisTypedPath <: TypedPath

    def typedPathCompanion: TypedPath.Companion[ThisTypedPath]
  }
}
