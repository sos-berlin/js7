package js7.data.filebased

import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.deriveCodec
/**
  * @author Joacim Zschimmer
  */
trait FileBasedsOverview {
  def count: Int
}

object FileBasedsOverview {
  final case class Standard(count: Int) extends FileBasedsOverview
  object Standard {
    implicit val jsonCodec = deriveCodec[Standard]
  }

  trait Companion[A <: FileBased] {
    type Overview <: FileBasedsOverview
    implicit def jsonCodec: CirceCodec[Overview]
    def fileBasedsToOverview(fileBaseds: Seq[A]): Overview
  }
}
