package js7.core.item

import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.data.item.{InventoryItem, SimpleItem, SourceType}

final class SimpleItemReader private(
  val companion: SimpleItem.Companion_)
extends ItemReader:

  def read(key: companion.Key, source: ByteArray)
  : PartialFunction[SourceType, Checked[InventoryItem]] =
    case SourceType.Json =>
      source
        .parseJsonAs(companion.jsonDecoder)
        .map(item => item
          .rename(key.asInstanceOf[item.companion.Key]))

object SimpleItemReader:
  def apply[I <: SimpleItem](implicit I: SimpleItem.Companion[I]): SimpleItemReader =
    new SimpleItemReader(I)
