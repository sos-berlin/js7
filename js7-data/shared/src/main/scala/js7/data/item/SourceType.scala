package js7.data.item


/**
  * @author Joacim Zschimmer
  */
sealed trait SourceType

object SourceType:
  sealed trait JsonLike extends SourceType

  case object Json extends JsonLike:
    override def toString = "JSON"

  case object Txt extends SourceType:
    override def toString = "txt"

  case object Xml extends SourceType:
    override def toString = "XML"

  val values = IndexedSeq(Json, Txt, Xml)
