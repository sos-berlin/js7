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
  final case class VersionAdded(version: FileBasedVersion) extends RepoEvent

  sealed trait FileBasedEvent extends RepoEvent {
    def path: TypedPath
  }

  sealed trait FileBasedAddedOrChanged extends FileBasedEvent {
    def fileBased: FileBased
  }
  object FileBasedAddedOrChanged {
    def unapply(o: FileBasedAddedOrChanged) = Some(o.fileBased)
  }

  final case class FileBasedAdded(fileBased: FileBased) extends FileBasedAddedOrChanged {
    def path = fileBased.path
  }

  final case class FileBasedChanged(fileBased: FileBased) extends FileBasedAddedOrChanged {
    def path = fileBased.path
  }

  final case class FileBasedDeleted(path: TypedPath) extends FileBasedEvent

  implicit def jsonCodec(implicit w: ObjectEncoder[FileBased], x: Decoder[FileBased], y: Encoder[TypedPath], z: Decoder[TypedPath])
  : TypedJsonCodec[RepoEvent] = TypedJsonCodec(
      Subtype(deriveCodec[VersionAdded]),
      Subtype(deriveCodec[FileBasedAdded]),
      Subtype(deriveCodec[FileBasedChanged]),
      Subtype(deriveCodec[FileBasedDeleted]))
}
