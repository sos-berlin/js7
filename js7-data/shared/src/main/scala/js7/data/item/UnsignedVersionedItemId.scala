package js7.data.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.UnsignedVersionedItemId.*
import scala.language.implicitConversions

final case class UnsignedVersionedItemId[P <: VersionedControlPath](path: P, versionId: VersionId)
extends UnsignedItemKey:
  def companion: InventoryItemKey.Companion[UnsignedVersionedItemId[P]] =
    path.companion.VersionedControlIdCompanion
      .asInstanceOf[InventoryItemKey.Companion[UnsignedVersionedItemId[P]]]

  def requireNonAnonymous(): this.type =
    path.requireNonAnonymous()
    versionId.requireNonAnonymous()
    this

  def toTypedString: String =
    path.toTypedString + VersionSeparator + versionId.string

  def toSimpleString =
    if versionId.isAnonymous then
      path.string
    else
      s"${path.string}$VersionSeparator${versionId.string}"

  def isAssignableToAgent =
    path.isAssignableToAgent

  override def toString =
    if versionId.isAnonymous then
      path.toString
    else
      s"${path.toTypedString}$VersionSeparator${versionId.string}"

  def pretty =
    if versionId.isAnonymous then
      path.string
    else
      s"${path.toTypedString}$VersionSeparator${versionId.string}"

object UnsignedVersionedItemId:
  val VersionSeparator = "~"  // Can be used in an Akka actor name

  // TODO Use this implicit conversion only for tests
  implicit def pathToItemId[P <: VersionedControlPath: js7.data.item.VersionedControlPath.Companion](path: P): UnsignedVersionedItemId[P] =
    UnsignedVersionedItemId(path, VersionId.Anonymous)

  implicit def ordering[P <: VersionedControlPath]: Ordering[UnsignedVersionedItemId[P]] =
    (a, b) => a.path.string.compareTo(b.path.string) match
      case 0 => -VersionId.ordering.compare(a.versionId, b.versionId)
      case o => o

  implicit def jsonEncoder[P <: VersionedControlPath: Encoder]: Encoder.AsObject[UnsignedVersionedItemId[P]] =
    o => JsonObject(
      "path" -> o.path.asJson,
      "versionId" -> o.versionId.asJson)

  implicit def jsonDecoder[P <: VersionedControlPath: VersionedControlPath.Companion: Decoder]: Decoder[UnsignedVersionedItemId[P]] =
    cursor =>
      for
        path <- cursor.get[P]("path")
        version <- cursor.get[VersionId]("versionId")
      yield UnsignedVersionedItemId(path, version)

  trait Companion[P <: VersionedControlPath]
  extends UnsignedItemKey.Companion[UnsignedVersionedItemId[P]]:
    implicit val pathCompanion: VersionedControlPath.Companion[P]
    private lazy val P = pathCompanion

    def apply(path: P, versionId: VersionId): UnsignedVersionedItemId[P]

    object as:
      def unapply(id: VersionedControlId_): Option[UnsignedVersionedItemId[P]] =
        (id.path.companion eq pathCompanion) ? id.asInstanceOf[UnsignedVersionedItemId[P]]

    final lazy val itemTypeName = P.itemTypeName

    implicit final val implicitCompanion: Companion[P] =
      this

    final def checked(string: String): Checked[UnsignedVersionedItemId[P]] =
      string indexOf VersionSeparator match
        case -1 => Problem(s"${P.name} without version (denoted by '$VersionSeparator')?: $string")
        case i => Right(apply(P(string take i), VersionId(string drop i + 1)))

    implicit final lazy val jsonEncoder: Encoder.AsObject[UnsignedVersionedItemId[P]] =
      implicit val x: Encoder[P] = P.jsonEncoder
      o => JsonObject(
        "path" -> o.path.asJson,
        "versionId" -> o.versionId.asJson)

    implicit final lazy val jsonDecoder: Decoder[UnsignedVersionedItemId[P]] =
      implicit val x: Decoder[P] = P.jsonDecoder
      cursor =>
        for
          path <- cursor.get[P]("path")
          version <- cursor.get[VersionId]("versionId")
        yield path ~ version
