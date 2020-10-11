package js7.data.item

import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.deriveCodec

/**
  * @author Joacim Zschimmer
  */
trait InventoryItemOverview {
  def count: Int
}

object InventoryItemOverview {
  final case class Standard(count: Int) extends InventoryItemOverview
  object Standard {
    implicit val jsonCodec = deriveCodec[Standard]
  }

  trait Companion[A <: InventoryItem] {
    type Overview <: InventoryItemOverview
    implicit def jsonCodec: CirceCodec[Overview]
    def itemsToOverview(items: Seq[A]): Overview
  }
}
