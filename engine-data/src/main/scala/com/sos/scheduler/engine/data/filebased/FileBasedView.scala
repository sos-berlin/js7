package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.common.HasViewCompanion

/**
  * @author Joacim Zschimmer
  */
trait FileBasedView {
  def asTyped[P <: TypedPath: TypedPath.Companion]: FileBasedView
}

object FileBasedView extends HasViewCompanion.WithKnownSubtypes[FileBasedView] {
  protected val subtypes: Subtypes = Set(FileBasedOverview, FileBasedDetailed)
}
