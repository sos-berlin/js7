package js7.data.item

import cats.implicits.toFlatMapOps
import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.SignedString
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.{DurationRichInt, RichDeadline}
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.{ByteArrayToLinesObservable, SetOnce}
import js7.data.crypt.VersionedItemVerifier.Verified
import monix.eval.Task
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now

sealed trait ItemOperation

object ItemOperation
{
  sealed trait SimpleItemOperation extends ItemOperation

  final case class SimpleAddOrReplace(item: SimpleItem)
  extends SimpleItemOperation

  final case class SimpleDelete(id: SimpleItemId)
  extends SimpleItemOperation

  sealed trait UpdateRepoOperation extends ItemOperation
  object UpdateRepoOperation
  {
    implicit def jsonCodec(implicit
      idJsonEncoder: Encoder[SimpleItemId],
      idJsonDecoder: Decoder[SimpleItemId],
      itemPathJsonEncoder: Encoder[ItemPath],
      itemPathJsonDecoder: Decoder[ItemPath])
    : TypedJsonCodec[UpdateRepoOperation] =
      TypedJsonCodec(
        Subtype(deriveCodec[AddVersion]),
        Subtype(deriveCodec[VersionedAddOrReplace]),
        Subtype(deriveCodec[VersionedDelete]))
  }

  final case class AddVersion(versionId: VersionId)
  extends UpdateRepoOperation

  sealed trait VersionedItemOperation extends UpdateRepoOperation

  final case class VersionedAddOrReplace(signedString: SignedString)
  extends VersionedItemOperation

  final case class VersionedDelete(path: ItemPath)
  extends VersionedItemOperation

  implicit def jsonCodec(implicit
    idJsonEncoder: Encoder[SimpleItemId],
    idJsonDecoder: Decoder[SimpleItemId],
    itemPathJsonEncoder: Encoder[ItemPath],
    itemPathJsonDecoder: Decoder[ItemPath],
    simpleItemJsonEncoder: Encoder[SimpleItem],
    simpleItemJsonDecoder: Decoder[SimpleItem])
  : TypedJsonCodec[ItemOperation] =
    TypedJsonCodec(
      Subtype(deriveCodec[SimpleAddOrReplace]),
      Subtype(deriveCodec[SimpleDelete]),
      Subtype[UpdateRepoOperation])
}
