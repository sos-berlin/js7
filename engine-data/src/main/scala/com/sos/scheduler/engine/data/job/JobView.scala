package com.sos.scheduler.engine.data.job

import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
trait JobView {
  def path: JobPath
}

object JobView {
  implicit def jsonFormat[V <: JobView: Companion]: RootJsonFormat[V] =
    companion[V].jsonFormat

  def companion[V <: JobView: Companion]: Companion[V] =
    implicitly[Companion[V]]

  trait Companion[V <: JobView] {
    implicit def jsonFormat: RootJsonFormat[V]

    implicit final def implicitCompanion: Companion[V] = this

    val name = getClass.getSimpleName stripSuffix "$"
  }
}
