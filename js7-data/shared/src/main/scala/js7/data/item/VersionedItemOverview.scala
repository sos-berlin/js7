package js7.data.item

import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.deriveCodec

/**
  * @author Joacim Zschimmer
  */
trait VersionedItemOverview {
  def count: Int
}

object VersionedItemOverview {
  final case class Standard(count: Int) extends VersionedItemOverview
  object Standard {
    implicit val jsonCodec = deriveCodec[Standard]
  }

  trait Companion[A <: VersionedItem] {
    type Overview <: VersionedItemOverview
    implicit def jsonCodec: CirceCodec[Overview]
    def itemsToOverview(items: Seq[A]): Overview
  }
}
