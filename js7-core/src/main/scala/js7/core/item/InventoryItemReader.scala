package js7.core.item

import io.circe.Json
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.data.ByteArray
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.common.http.CirceToYaml.yamlToJson
import js7.core.item.InventoryItemReader._
import js7.data.item.{InventoryItem, ItemId, ItemId_, SourceType, ItemPath}

/**
  * @author Joacim Zschimmer
  */
trait InventoryItemReader
{
  val companion: InventoryItem.Companion_

  import companion.{ThisItem, Path => ThisItemPath}

  protected def read(id: ItemId[ThisItemPath], byteArray: ByteArray): PartialFunction[SourceType, Checked[ThisItem]]

  def convertFromJson(json: Json): Checked[ThisItem]

  private[item] def readUntyped(id: ItemId_, byteArray: ByteArray, sourceType: SourceType): Checked[ThisItem] = {
    assertThat(id.path.companion eq itemPathCompanion, "InventoryItemReader readUntyped")
    val result: Checked[ThisItem] = read(id.asInstanceOf[ItemId[ThisItemPath]], byteArray).applyOrElse(sourceType,
      (_: SourceType) => Problem(s"Unrecognized SourceType '$sourceType' for path '$id'"))
    result.mapProblem(p => SourceProblem(id.path, sourceType, p))
  }

  final def readJsonString(source: String): Checked[ThisItem] =
    source.parseJsonChecked flatMap convertFromJson

  final def readAnonymousJsonLike(sourceType: SourceType.JsonLike, source: ByteArray): Checked[ThisItem] =
    sourceType match {
      case SourceType.Json =>
        readJsonString(source.utf8String)

      case SourceType.Yaml =>
        yamlToJson(source.utf8String) flatMap convertFromJson
    }

  private[item] def itemPathCompanion: ItemPath.Companion[ThisItemPath] = companion.itemPathCompanion
}

object InventoryItemReader
{
  final case class SourceProblem private(path: ItemPath, sourceType: SourceType, underlying: Problem)
    extends Problem.Lazy(s"Problem with '$path' ($sourceType)", Some(underlying))
}
