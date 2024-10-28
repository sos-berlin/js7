package js7.data.item

import io.circe.{Codec, DecodingFailure, HCursor, Json}
import java.nio.file.{Path, Paths}
import java.util.Locale
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.base.standards.Js7PathValidating
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.checkedCast
import js7.data.item.InventoryItemPath.*
import scala.reflect.ClassTag

trait InventoryItemPath extends GenericString:
  def companion: Companion[? <: InventoryItemPath]

  final def isAssignableToAgent: Boolean =
    this.isInstanceOf[InventoryItemPath.AttachableToAgent]

  final def toTypedString: String =
    s"${companion.pathTypeName}:$string"

  /** The relative standard source file path. */
  def toFile(t: SourceType): Path =
    Paths.get(string + companion.sourceTypeToFilenameExtension(t))

  override def toString = toTypedString


object InventoryItemPath:
  implicit val inventoryItemPathOrdering: Ordering[InventoryItemPath] =
    GenericString.ordering[InventoryItemPath]

  abstract class Companion[P <: InventoryItemPath: ClassTag] extends Js7PathValidating[P]:
    private lazy val itemTypeName_ =
      if name.endsWith("Path")
      then name.stripSuffix("Path")
      else name.stripSuffix("Id")

    def itemTypeName: String = itemTypeName_

    def itemTypeNameAliases: Seq[String] = Nil

    def pathTypeName: String = itemTypeName

    private lazy val defaultSourceTypeToFilenameExtension: Map[SourceType, String] =
      Map(SourceType.Json -> ("." + pathTypeName.toLowerCase(Locale.ROOT) + ".json"))

    def sourceTypeToFilenameExtension: Map[SourceType, String] =
      defaultSourceTypeToFilenameExtension

    /** Converts a relative file path with normalized slahes (/) to a `VersionedItemPath`. */
    final def fromFile(normalized: String): Option[Checked[(P, SourceType)]] =
      sourceTypeToFilenameExtension
        .collectFirst:
          case (t, ext) if normalized.endsWith(ext) =>
            checked(normalized.dropRight(ext.length)).map(_ -> t)

    final def toPossibleFilenames(path: P): Iterable[String] =
      sourceTypeToFilenameExtension.values.map(path.toString + _)

  type AnyCompanion = Companion[? <: InventoryItemPath]

  trait AttachableToAgent:
    this: InventoryItemPath =>

  def jsonCodec[P <: InventoryItemPath: ClassTag](companions: Iterable[Companion[? <: P]]): Codec[P] =
    new Codec[P]:
      private val typeToCompanion = companions.toKeyedMap(_.pathTypeName)

      def apply(a: P) = Json.fromString(a.toTypedString)

      def apply(c: HCursor) =
        for
          string <- c.as[String]
          prefixAndPath <- string.indexOf(':') match
            case i if i > 0 => Right((string.take(i), string.substring(i + 1)))
            case _ => Left(DecodingFailure(s"Missing type prefix in InventoryItemPath: $string", c.history))
          prefix = prefixAndPath._1
          path = prefixAndPath._2
          itemPath <- typeToCompanion
            .get(prefix)
            .toRight(Problem.pure(s"Unrecognized type prefix in InventoryItemPath: $prefix"))
            .flatMap(_.checked(path))
            .flatMap(checkedCast[P](_))
            .toDecoderResult(c.history)
        yield itemPath
