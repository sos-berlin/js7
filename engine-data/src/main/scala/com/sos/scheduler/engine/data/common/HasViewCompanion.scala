package com.sos.scheduler.engine.data.common

import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
trait HasViewCompanion[Super] {

  implicit def jsonFormat[V <: Super: Companion]: RootJsonFormat[V] =
    companion[V].jsonFormat

  implicit def superJsonFormat[V <: Super: Companion]: RootJsonFormat[Super] =
    companion[V].jsonFormat.asInstanceOf[RootJsonFormat[Super]]

  def companion[V <: Super: Companion]: Companion[V] =
    implicitly[Companion[V]]

  type AnyCompanion[V] = Companion[_ <: V]

  type Companion[V <: Super] = HasViewCompanion.Companion[V]
}

object HasViewCompanion {

  trait Companion[V] {
    implicit def jsonFormat: RootJsonFormat[V]

    val name = getClass.getSimpleName stripSuffix "$"

    implicit final def implicitCompanion: Companion[V] = this
  }

  trait WithKnownSubtypes[Super] extends HasViewCompanion[Super] {
    protected type Subtypes = Set[Companion[_ <: Super]]

    protected def subtypes: Subtypes

    private lazy val nameToCompanion: Map[String, Companion[_ <: Super]] =
      (subtypes map { o ⇒ o.name → o }).toMap

    def companion(name: String): Either[String, Companion[_ <: Super]] =
      nameToCompanion.get(name) match {
        case Some(o) ⇒ Right(o)
        case None ⇒ Left(s"Unknown view '$name'. Valid views are: ${(subtypes map { _.name }) mkString ", "}")
      }
  }
}
