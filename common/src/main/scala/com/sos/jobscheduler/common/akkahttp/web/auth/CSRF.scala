package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.HttpMethods.POST
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.MediaTypes.{`application/x-www-form-urlencoded`, `multipart/form-data`, `text/plain`}
import akka.http.scaladsl.model.StatusCodes.Forbidden
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives._

/**
  * Simplistic check agains some CSRF attacks, especially HTML 5 form POST.
  * https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)
  * <p>
  *   See <a href="https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)">https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)</a>.
  *
  * @author Joacim Zschimmer
  */
object CSRF
{
  /** For a list of cross-site HTML 5 form POST content types, see https://www.w3.org/TR/html5/forms.html#attr-fs-enctype. */
  private val RejectedContentTypes = Set(
    `application/x-www-form-urlencoded`, `multipart/form-data`, `text/plain`).map(_.value) +
    "none/none"

  val forbidCSRF: Directive0 =
    mapInnerRoute { inner =>
      extractRequest { request =>
        if (looksLikeHtmlFormPost(request))
          complete(Forbidden)   // be quiet: -> "HTML form POST is forbidden")
        else
          inner
      }
    }

  private def looksLikeHtmlFormPost(request: HttpRequest): Boolean =
    request.method == POST &&
      RejectedContentTypes.contains(request.entity.contentType.mediaType.value)
}
