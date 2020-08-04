package js7.core.item

import akka.util.ByteString
import io.circe.Json
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.common.http.CirceToYaml.yamlToJson
import js7.core.item.InventoryItemReader._
import js7.data.item.{InventoryItem, ItemId, SourceType, TypedPath, ItemId_}

/**
  * @author Joacim Zschimmer
  */
trait InventoryItemReader
{
  val companion: InventoryItem.Companion_

  import companion.{ThisItem, Path => ThisTypedPath}

  protected def read(id: ItemId[ThisTypedPath], byteString: ByteString): PartialFunction[SourceType, Checked[ThisItem]]

  def convertFromJson(json: Json): Checked[ThisItem]

  private[item] def readUntyped(id: ItemId_, byteString: ByteString, sourceType: SourceType): Checked[ThisItem] = {
    assertThat(id.path.companion eq typedPathCompanion, "InventoryItemReader readUntyped")
    val result: Checked[ThisItem] = read(id.asInstanceOf[ItemId[ThisTypedPath]], byteString).applyOrElse(sourceType,
      (_: SourceType) => Problem(s"Unrecognized SourceType '$sourceType' for path '$id'"))
    result.mapProblem(p => SourceProblem(id.path, sourceType, p))
  }

  final def readJsonString(source: String): Checked[ThisItem] =
    source.parseJsonChecked flatMap convertFromJson

  final def readAnonymousJsonLike(sourceType: SourceType.JsonLike, source: ByteString): Checked[ThisItem] =
    sourceType match {
      case SourceType.Json =>
        readJsonString(source.utf8String)

      case SourceType.Yaml =>
        yamlToJson(source.utf8String) flatMap convertFromJson
    }

  private[item] def typedPathCompanion: TypedPath.Companion[ThisTypedPath] = companion.typedPathCompanion
}

object InventoryItemReader
{
  final case class SourceProblem private(path: TypedPath, sourceType: SourceType, underlying: Problem)
    extends Problem.Lazy(s"Problem with '$path' ($sourceType)", Some(underlying))
}
