package js7.data.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.VersionedItemId._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class VersionedItemId[+P <: ItemPath](path: P, versionId: VersionId)
{
  def requireNonAnonymous(): this.type = {
    path.requireNonAnonymous()
    versionId.requireNonAnonymous()
    this
  }

  def isAnonymous = path.isAnonymous && versionId.isAnonymous

  def toSimpleString = if (versionId.isAnonymous) path.string else s"${path.string}$VersionSeparator${versionId.string}"

  override def toString = if (versionId.isAnonymous) path.toString else s"${path.toTypedString}$VersionSeparator${versionId.string}"

  def pretty = if (versionId.isAnonymous) path.string else s"${path.toTypedString}$VersionSeparator${versionId.string}"
}

object VersionedItemId
{
  val VersionSeparator = "~"  // Can be used in an Akka actor name

  implicit def pathToItemId[P <: ItemPath](path: P): VersionedItemId[P] =
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

  trait Companion[P <: ItemPath] {
    //def checked(string: String)(implicit P: ItemPath.Companion[P]): Checked[VersionedItemId[P]] =
    //  string indexOf VersionSeparator match {
    //    case -1 => Problem(s"ItemIdPath without version (denoted by '$VersionSeparator')?: $string")
    //    case i => Right(new VersionedItemId(P(string take i), VersionId(string drop i + 1)))
    //  }
  }
}
