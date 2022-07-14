package js7.core.item

import js7.base.data.ByteArray
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.core.item.ItemReader.*
import js7.data.item.{InventoryItem, InventoryItemPath, SourceType}

trait ItemReader
{
  val companion: InventoryItem.Companion_

  import companion.Path as ThisItemPath

  private[item] final lazy val itemPathCompanion: InventoryItemPath.Companion[ThisItemPath] =
    companion.Path

  protected def read(key: companion.Key, source: ByteArray)
  : PartialFunction[SourceType, Checked[InventoryItem]]

  private[item] final def readUntyped(key: companion.Key, byteArray: ByteArray, sourceType: SourceType)
  : Checked[InventoryItem] = {
    assertThat(key.path.companion eq itemPathCompanion, "VersionedItemReader readUntyped")
    val result: Checked[InventoryItem] =
      read(key, byteArray)
        .applyOrElse(
          sourceType,
          (_: SourceType) => Problem(s"Unrecognized SourceType '$sourceType' for path '$key'"))
    result.mapProblem(p => SourceProblem(key.path, sourceType, p))
  }
}

object ItemReader
{
  final case class SourceProblem(
    path: InventoryItemPath,
    sourceType: SourceType,
    underlying: Problem)
  extends Problem.Lazy(s"Problem with '$path' ($sourceType)", Some(underlying))
}
