package com.sos.jobscheduler.master.web.simplegui

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.akkahttp.html.WebServiceContext
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext._

/**
  * @author Joacim Zschimmer
  */
final class MasterWebServiceContext(val htmlEnabled: Boolean = false)
extends WebServiceContext {

  private val baseUri = "/jobscheduler/master"

  def toWebjarUri(path: String): Uri = uri(s"api/frontend/webjars/$path")

  def uri(path: String): Uri = Uri(uriString(path))

  def uriString(path: String): String = s"$baseUri/$path"

  def staticFilesResource = StaticPackage
}

object MasterWebServiceContext {
  private val StaticPackage = JavaResource("com/sos/jobscheduler/master/web/simplegui/frontend/")
}
