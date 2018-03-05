package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass

/**
  * @author Joacim Zschimmer
  */
trait FileBased {
  type Self <: FileBased
  type Path = companion.Path
  type Id = FileBasedId[Path]

  val companion: FileBased.Companion[Self]
  def id: FileBasedId[Path]
  def withId(id: FileBasedId[Path]): Self

  final def path: Path = id.path

  final def isAnonymous = id.isAnonymous

  final def withoutVersion: Self = withVersion(VersionId.Anonymous)

  final def withoutId: Self = withId(id = companion.typedPathCompanion.NoId)

  final def withVersion(v: VersionId): Self = withId(id = id.copy(versionId = v))

  def cast[A <: FileBased](implicit A: FileBased.Companion[A]): A = {
    if (A != companion) throw new ClassCastException(s"Expected ${companion.typedPathCompanion.name} but is: $path")
    this.asInstanceOf[A]
  }
}

object FileBased {
  type Companion_ = Companion[_ <: FileBased]

  trait Companion[A <: FileBased] {
    type ThisFileBased <: A
    type Path <: TypedPath

    val name = getClass.simpleScalaName

    def typedPathCompanion: TypedPath.Companion[Path]

    implicit def self: Companion[A] = this

    override def toString = name
  }
}
