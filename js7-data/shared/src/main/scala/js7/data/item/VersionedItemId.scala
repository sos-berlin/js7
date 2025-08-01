package js7.data.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.VersionedItemId.*
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class VersionedItemId[P <: VersionedItemPath](path: P, versionId: VersionId)
extends SignableItemKey:
  def companion: InventoryItemKey.Companion[VersionedItemId[P]] =
    path.companion.VersionedItemIdCompanion
      .asInstanceOf[InventoryItemKey.Companion[VersionedItemId[P]]]

  def requireNonAnonymous(): this.type =
    path.requireNonAnonymous()
    versionId.requireNonAnonymous()
    this

  def toTypedString: String =
    path.toTypedString + VersionSeparator + versionId.string

  def isAnonymous: Boolean = path.isAnonymous && versionId.isAnonymous

  def toSimpleString: String =
    if versionId.isAnonymous then
      path.string
    else
      s"${path.string}$VersionSeparator${versionId.string}"

  def isAssignableToAgent: Boolean =
    path.isAssignableToAgent

  override def toString: String =
    if versionId.isAnonymous then
      path.toString
    else
      s"${path.toTypedString}$VersionSeparator${versionId.string}"

  def pretty: String =
    if versionId.isAnonymous then
      path.string
    else
      s"${path.toTypedString}$VersionSeparator${versionId.string}"


object VersionedItemId:
  val VersionSeparator = "~"  // Can be used in an Pekko actor name

  // TODO Use this implicit convertion only for tests
  implicit def pathToItemId[P <: VersionedItemPath](path: P): VersionedItemId[P] =
    VersionedItemId(path, VersionId.Anonymous)

  implicit def ordering[P <: VersionedItemPath]: Ordering[VersionedItemId[P]] =
    (a, b) => a.path.string.compareTo(b.path.string) match
      case 0 => -VersionId.ordering.compare(a.versionId, b.versionId)
      case o => o

  implicit def jsonEncoder[P <: VersionedItemPath: Encoder]: Encoder.AsObject[VersionedItemId[P]] =
    o => JsonObject(
      "path" -> (!o.path.isAnonymous ? o.path).asJson,
      "versionId" -> (!o.versionId.isAnonymous ? o.versionId).asJson)

  implicit def jsonDecoder[P <: VersionedItemPath: VersionedItemPath.Companion: Decoder]: Decoder[VersionedItemId[P]] =
    cursor =>
      for
        path <- cursor.getOrElse[P]("path")(implicitly[VersionedItemPath.Companion[P]].Anonymous)
        version <- cursor.getOrElse[VersionId]("versionId")(VersionId.Anonymous)
      yield VersionedItemId(path, version)

  trait Companion[P <: VersionedItemPath] extends SignableItemKey.Companion[VersionedItemId[P]]:
    implicit val pathCompanion: VersionedItemPath.Companion[P]
    private lazy val P = pathCompanion

    def apply(path: P, versionId: VersionId): VersionedItemId[P]

    object as:
      def unapply(id: VersionedItemId_): Option[VersionedItemId[P]] =
        (id.path.companion eq pathCompanion) ? id.asInstanceOf[VersionedItemId[P]]

    final lazy val itemTypeName = P.itemTypeName

    protected def itemTypeNameAliases: Seq[String] = Nil

    implicit final val implicitCompanion: Companion[P] =
      this

    final def checked(string: String): Checked[VersionedItemId[P]] =
      string.indexOf(VersionSeparator) match
        case -1 => Problem(s"${P.name} without version (denoted by '$VersionSeparator')?: $string")
        case i => Right(apply(P(string take i), VersionId(string drop i + 1)))

    implicit final lazy val jsonEncoder: Encoder.AsObject[VersionedItemId[P]] =
      implicit val x: Encoder[P] = P.jsonEncoder
      o => JsonObject(
        "path" -> (!o.path.isAnonymous ? o.path).asJson,
        "versionId" -> (!o.versionId.isAnonymous ? o.versionId).asJson)

    implicit final lazy val jsonDecoder: Decoder[VersionedItemId[P]] =
      implicit val x: Decoder[P] = P.jsonDecoder
      cursor =>
        for
          path <- cursor.getOrElse[P]("path")(implicitly[VersionedItemPath.Companion[P]].Anonymous)
          version <- cursor.getOrElse[VersionId]("versionId")(VersionId.Anonymous)
        yield path ~ version
