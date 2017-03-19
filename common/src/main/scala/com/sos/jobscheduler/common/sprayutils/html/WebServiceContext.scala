package com.sos.jobscheduler.common.sprayutils.html

import com.sos.jobscheduler.common.utils.JavaResource
import spray.http.Uri

/**
  * @author Joacim Zschimmer
  */
trait WebServiceContext {

  final lazy val htmlIncluder = new HtmlIncluder(this)

  def htmlEnabled: Boolean

  def toWebjarUri(path: String): Uri

  def staticFilesResource: JavaResource
}
