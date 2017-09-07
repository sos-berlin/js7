package com.sos.jobscheduler.common.akkahttp.web.auth

import akka.http.scaladsl.model.HttpMethods.POST
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.MediaTypes.{`application/x-www-form-urlencoded`, `multipart/form-data`, `text/plain`}
import akka.http.scaladsl.model.StatusCodes.Forbidden
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.common.akkahttp.web.auth.CSRF._
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import scala.collection.JavaConversions._

/**
  * Simplistic check agains some CSRF attacks, especially HTML 5 form POST.
  * https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)
  * <p>
  *   See <a href="https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)">https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)</a>.
  *
  * @author Joacim Zschimmer
  */
@Singleton
final class CSRF @Inject()(conf: Configuration) {

  val rejectSomeCSRF: Directive0 =
    mapInnerRoute { inner ⇒
      extractRequest { request ⇒
        if (looksLikeHtmlFormPost(request))
          complete(Forbidden → "HTML form POST is forbidden")
        else
          inner
      }
    }

  private def looksLikeHtmlFormPost(request: HttpRequest): Boolean =
    request.method == POST &&
      conf.rejectedContentTypes.contains(request.entity.contentType.mediaType.value)
}

object CSRF {

  final case class Configuration(
    /** For a list of cross-site HTML 5 form POST content types, see https://www.w3.org/TR/html5/forms.html#attr-fs-enctype. */
    rejectedContentTypes: Set[String])

  object Configuration {
    // See https://www.w3.org/TR/html5/forms.html#attr-fs-enctype
    @Deprecated
    val ForTest = Configuration(
      rejectedContentTypes = Set(`application/x-www-form-urlencoded`, `multipart/form-data`, `text/plain`) map { _.value })

    def fromSubConfig(config: Config) = new Configuration(
      rejectedContentTypes = Set("none/none") ++ config.getStringList("reject-post-content-types"))
  }
}
