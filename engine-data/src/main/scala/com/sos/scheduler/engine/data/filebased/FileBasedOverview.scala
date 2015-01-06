package com.sos.scheduler.engine.data.filebased

trait FileBasedOverview {
  def path: TypedPath
  def fileBasedState: FileBasedState
}
