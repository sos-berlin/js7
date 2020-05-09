package com.sos.jobscheduler.common.http

import com.sos.jobscheduler.base.web.{HttpClient, Uri}

trait GenericHttpClientFactory
{
  def apply(baseUri: Uri, uriPrefixPath: String, name: String): HttpClient
}
