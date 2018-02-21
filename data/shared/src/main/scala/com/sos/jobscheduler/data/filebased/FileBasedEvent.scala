package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import io.circe.{Decoder, Encoder}

/**
  * @author Joacim Zschimmer
  */
sealed trait FileBasedEvent {
  def path: TypedPath
}

object FileBasedEvent {
  final case class FileBasedAdded(fileBased: FileBased) extends FileBasedEvent {
    def path = fileBased.path
  }

  final case class FileBasedChanged(fileBased: FileBased) extends FileBasedEvent {
    def path = fileBased.path
  }

  final case class FileBasedDeleted(path: TypedPath) extends FileBasedEvent

  implicit def jsonCodec(implicit w: Encoder[FileBased], x: Decoder[FileBased], y: Encoder[TypedPath], z: Decoder[TypedPath])
  : TypedJsonCodec[FileBasedEvent] = TypedJsonCodec(
      Subtype(deriveCodec[FileBasedAdded]),
      Subtype(deriveCodec[FileBasedChanged]),
      Subtype(deriveCodec[FileBasedDeleted]))
}
