package com.sos.jobscheduler.data.filebased

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
trait FileBased {
  type Self <: FileBased

  def path: TypedPath
  def companion: FileBased.Companion[Self]
}

object FileBased {
  type Companion_ = Companion[_ <: FileBased]

  trait Companion[A <: FileBased] {
    type ThisFileBased <: A
    type ThisTypedPath <: TypedPath
    type Versioned = FileBased.Versioned[A]

    def typedPathCompanion: TypedPath.Companion[ThisTypedPath]

    implicit def self: Companion[A] = this
  }

  type Versioned_ = Versioned[FileBased]

  final case class Versioned[A <: FileBased](version: FileBasedVersion, fileBased: A) {
    def versionedPath = fileBased.path.companion.Versioned(version, fileBased.path)
  }
  object Versioned {
    implicit def jsonEncoder[A <: FileBased: ObjectEncoder]: ObjectEncoder[Versioned[A]] =
      o ⇒ ("version" → o.version.asJson) +: o.fileBased.asJsonObject

    implicit def jsonDecoder[A <: FileBased: Decoder]: Decoder[Versioned[A]] =
      cursor ⇒
        for {
          version ← cursor.get[FileBasedVersion]("version")
          fileBased ← cursor.as[A]
        } yield Versioned(version, fileBased)
  }
}
