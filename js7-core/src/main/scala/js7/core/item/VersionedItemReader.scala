package js7.core.item

import io.circe.Json
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.data.ByteArray
import js7.base.io.yaml.YamlExtensions.yamlToJson
import js7.base.problem.{Checked, Problem}
import js7.data.item.{SourceType, VersionedItem, VersionedItemId, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
trait VersionedItemReader extends ItemReader:
  val companion: VersionedItem.Companion_

  import companion.Item
  private type ThisItemPath = companion.Path

  protected def read(id: VersionedItemId[ThisItemPath], byteArray: ByteArray): PartialFunction[SourceType, Checked[Item]]

  def convertFromJson(json: Json): Checked[Item]

  //private[item] def readUntyped(id: VersionedItemId_, byteArray: ByteArray, sourceType: SourceType): Checked[Item] = {
  //  assertThat(id.path.companion eq itemPathCompanion, "VersionedItemReader readUntyped")
  //  val result: Checked[Item] = read(id.asInstanceOf[VersionedItemId[ThisItemPath]], byteArray).applyOrElse(sourceType,
  //    (_: SourceType) => Problem(s"Unrecognized SourceType '$sourceType' for path '$id'"))
  //  result.mapProblem(p => SourceProblem(id.path, sourceType, p))
  //}

  final def readAnonymousJsonLike(sourceType: SourceType.JsonLike, source: ByteArray): Checked[Item] =
    sourceType match
      case SourceType.Json => source.utf8String.parseJson flatMap convertFromJson
      case SourceType.Yaml => source.utf8String.yamlToJson flatMap convertFromJson


object VersionedItemReader:
  final case class SourceProblem(path: VersionedItemPath, sourceType: SourceType, underlying: Problem)
    extends Problem.Lazy(s"Problem with '$path' ($sourceType)", Some(underlying))
