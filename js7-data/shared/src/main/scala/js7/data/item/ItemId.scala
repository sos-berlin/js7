package js7.data.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.ItemId._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class ItemId[+P <: TypedPath](path: P, versionId: VersionId)
{
  def requireNonAnonymous(): this.type = {
    path.requireNonAnonymous()
    versionId.requireNonAnonymous()
    this
  }

  def isAnonymous = path.isAnonymous && versionId.isAnonymous

  def toSimpleString = if (versionId.isAnonymous) path.string else s"${path.string}$VersionSeparator${versionId.string}"

  override def toString = if (versionId.isAnonymous) path.toString else s"$path$VersionSeparator${versionId.string}"

  def pretty = if (versionId.isAnonymous) path.string else s"${path.string}$VersionSeparator${versionId.string}"
}

object ItemId
{
  val VersionSeparator = "~"  // Can be used in an Akka actor name

  implicit def pathToItemId[P <: TypedPath](path: P): ItemId[P] =
    ItemId(path, VersionId.Anonymous)

  implicit def ordering[P <: TypedPath]: Ordering[ItemId[P]] =
    Ordering.by(o => (o.path, o.versionId))

  implicit def jsonEncoder[P <: TypedPath: Encoder]: Encoder.AsObject[ItemId[P]] =
    o => JsonObject(
      "path" -> (!o.path.isAnonymous ? o.path).asJson,
      "versionId" -> (!o.versionId.isAnonymous ? o.versionId).asJson)

  implicit def jsonDecoder[P <: TypedPath: TypedPath.Companion: Decoder]: Decoder[ItemId[P]] =
    cursor =>
      for {
        path <- cursor.get[Option[P]]("path") map (_ getOrElse implicitly[TypedPath.Companion[P]].Anonymous)
        version <- cursor.get[Option[VersionId]]("versionId") map (_ getOrElse VersionId.Anonymous)
      } yield ItemId(path, version)

  trait Companion[P <: TypedPath] {
    //def checked(string: String)(implicit P: TypedPath.Companion[P]): Checked[ItemId[P]] =
    //  string indexOf VersionSeparator match {
    //    case -1 => Problem(s"ItemIdPath without version (denoted by '$VersionSeparator')?: $string")
    //    case i => Right(new ItemId(P(string take i), VersionId(string drop i + 1)))
    //  }
  }
}
