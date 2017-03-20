package com.sos.jobscheduler.master.web.simplegui

import com.sos.jobscheduler.common.sprayutils.html.WebServiceContext
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext._
import spray.http.Uri

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
