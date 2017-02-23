package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.data.filebased.TypedPath

/**
  * @author Joacim Zschimmer
  */
final case class OrderGeneratorPath(string: String) extends TypedPath {
  validate()

  def companion = OrderGeneratorPath
}

object OrderGeneratorPath extends TypedPath.Companion[OrderGeneratorPath] {

  override lazy val filenameExtension = ".order.xml"
}
