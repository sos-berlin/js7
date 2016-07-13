package com.sos.scheduler.engine.data.filebased

trait FileBasedOverview extends HasPath {
  def path: TypedPath
  def fileBasedState: FileBasedState
}
