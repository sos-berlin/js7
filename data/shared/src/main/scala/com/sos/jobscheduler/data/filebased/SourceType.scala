package com.sos.jobscheduler.data.filebased

import scala.collection.immutable.IndexedSeq

/**
  * @author Joacim Zschimmer
  */
sealed trait SourceType

object SourceType {
  case object Json extends SourceType {
    override def toString = "JSON"
  }

  case object Txt extends SourceType {
    override def toString = "txt"
  }

  case object Xml extends SourceType {
    override def toString = "XML"
  }

  val values = IndexedSeq(Json, Txt, Xml)
}
