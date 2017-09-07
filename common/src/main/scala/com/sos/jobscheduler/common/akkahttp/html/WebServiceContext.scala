package com.sos.jobscheduler.common.akkahttp.html

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.utils.JavaResource

/**
  * @author Joacim Zschimmer
  */
trait WebServiceContext {

  final lazy val htmlIncluder = new HtmlIncluder(this)

  def htmlEnabled: Boolean

  def toWebjarUri(path: String): Uri

  def staticFilesResource: JavaResource
}
