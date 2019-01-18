package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.NoKeyEvent
import io.circe.{Decoder, Encoder, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
sealed trait RepoEvent extends NoKeyEvent

object RepoEvent {
  final case class VersionAdded(versionId: VersionId) extends RepoEvent

  sealed trait FileBasedEvent extends RepoEvent {
    def path: TypedPath
  }

  sealed trait FileBasedAddedOrChanged extends FileBasedEvent with Product {
    def fileBased: FileBased
    final def path: FileBased#Path = id.path
    def id: FileBasedId[FileBased#Path] = fileBased.id
    def toShortString = s"$productPrefix($id)"
  }
  object FileBasedAddedOrChanged {
    def unapply(o: FileBasedAddedOrChanged) = Some(o.fileBased)
  }

  final case class FileBasedAdded(fileBased: FileBased) extends FileBasedAddedOrChanged {
    require(!fileBased.id.path.isAnonymous, "FileBasedAdded event requires a path")
    require(fileBased.id.versionId.isAnonymous, s"VersionId must be anonymous in $toString")
  }

  final case class FileBasedChanged(fileBased: FileBased) extends FileBasedAddedOrChanged {
    require(!fileBased.id.path.isAnonymous, "FileChangedChanged event requires a path")
    require(fileBased.id.versionId.isAnonymous, s"VersionId must be anonymous in $toString")
  }

  final case class FileBasedDeleted(path: TypedPath) extends FileBasedEvent {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }

  implicit def jsonCodec(implicit w: ObjectEncoder[FileBased], x: Decoder[FileBased], y: Encoder[TypedPath], z: Decoder[TypedPath])
  : TypedJsonCodec[RepoEvent] = TypedJsonCodec(
      Subtype(deriveCodec[VersionAdded]),
      Subtype(deriveCodec[FileBasedAdded]),
      Subtype(deriveCodec[FileBasedChanged]),
      Subtype(deriveCodec[FileBasedDeleted]))
}
