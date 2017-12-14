package com.sos.jobscheduler.common.akkahttp.html

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.utils.JavaResource

/**
  * @author Joacim Zschimmer
  */
trait WebServiceContext {

  def htmlEnabled: Boolean

  def toWebjarUri(path: String): Uri

  def staticFilesResource: JavaResource
}
