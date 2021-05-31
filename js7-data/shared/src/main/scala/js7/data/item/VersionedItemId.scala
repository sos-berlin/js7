package js7.data.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.VersionedItemId._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class VersionedItemId[P <: ItemPath](path: P, versionId: VersionId)
extends SignableItemKey
{
  def companion: InventoryItemKey.Companion[VersionedItemId[P]] =
    path.companion.VersionedItemIdCompanion
      .asInstanceOf[InventoryItemKey.Companion[VersionedItemId[P]]]

  def requireNonAnonymous(): this.type = {
    path.requireNonAnonymous()
    versionId.requireNonAnonymous()
    this
  }

  def toTypedString: String =
    path.companion.itemTypeName + ":" + path.string + VersionSeparator + versionId.string

  def isAnonymous = path.isAnonymous && versionId.isAnonymous

  def toSimpleString =
    if (versionId.isAnonymous)
      path.string
    else
      s"${path.string}$VersionSeparator${versionId.string}"

  override def toString =
    if (versionId.isAnonymous)
      path.toString
    else
      s"${path.toTypedString}$VersionSeparator${versionId.string}"

  def pretty =
    if (versionId.isAnonymous)
      path.string
    else
      s"${path.toTypedString}$VersionSeparator${versionId.string}"
}

object VersionedItemId
{
  val VersionSeparator = "~"  // Can be used in an Akka actor name

  // TODO Use this implicit convertion only for tests
  implicit def pathToItemId[P <: ItemPath: ItemPath.Companion](path: P): VersionedItemId[P] =
    VersionedItemId(path, VersionId.Anonymous)

  implicit def ordering[P <: ItemPath]: Ordering[VersionedItemId[P]] =
    Ordering.by(o => (o.path, o.versionId))

  implicit def jsonEncoder[P <: ItemPath: Encoder]: Encoder.AsObject[VersionedItemId[P]] =
    o => JsonObject(
      "path" -> (!o.path.isAnonymous ? o.path).asJson,
      "versionId" -> (!o.versionId.isAnonymous ? o.versionId).asJson)

  implicit def jsonDecoder[P <: ItemPath: ItemPath.Companion: Decoder]: Decoder[VersionedItemId[P]] =
    cursor =>
      for {
        path <- cursor.getOrElse[P]("path")(implicitly[ItemPath.Companion[P]].Anonymous)
        version <- cursor.getOrElse[VersionId]("versionId")(VersionId.Anonymous)
      } yield VersionedItemId(path, version)

  trait Companion[P <: ItemPath] extends SignableItemKey.Companion[VersionedItemId[P]]
  {
    implicit val pathCompanion: ItemPath.Companion[P]
    private lazy val P = pathCompanion

    def apply(path: P, versionId: VersionId): VersionedItemId[P]

    final lazy val itemTypeName = P.itemTypeName

    implicit final val implicitCompanion: Companion[P] =
      this

    final def checked(string: String): Checked[VersionedItemId[P]] =
      string indexOf VersionSeparator match {
        case -1 => Problem(s"${P.name} without version (denoted by '$VersionSeparator')?: $string")
        case i => Right(apply(P(string take i), VersionId(string drop i + 1)))
      }

    implicit final lazy val jsonEncoder: Encoder.AsObject[VersionedItemId[P]] = {
      implicit val x: Encoder[P] = P.jsonEncoder
      o => JsonObject(
        "path" -> (!o.path.isAnonymous ? o.path).asJson,
        "versionId" -> (!o.versionId.isAnonymous ? o.versionId).asJson)
    }

    implicit final lazy val jsonDecoder: Decoder[VersionedItemId[P]] = {
      implicit val x: Decoder[P] = P.jsonDecoder
      cursor =>
        for {
          path <- cursor.getOrElse[P]("path")(implicitly[ItemPath.Companion[P]].Anonymous)
          version <- cursor.getOrElse[VersionId]("versionId")(VersionId.Anonymous)
        } yield path ~ version
    }
  }
}
