package com.sos.scheduler.engine.data.processclass

import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
trait ProcessClassView

object ProcessClassView {
  trait Companion[V <: ProcessClassView] {
    implicit def jsonFormat: RootJsonFormat[V]

    implicit final def implicitCompanion: Companion[V] = this

    val name = getClass.getSimpleName stripSuffix "$"
  }

  implicit def jsonFormat[V <: ProcessClassView: ProcessClassView.Companion] = implicitly[ProcessClassView.Companion[V]].jsonFormat
}
