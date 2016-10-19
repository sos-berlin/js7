package com.sos.scheduler.engine.common.sprayutils.web.auth

import com.sos.scheduler.engine.common.sprayutils.SprayUtils._
import com.sos.scheduler.engine.common.sprayutils.web.auth.CSRF._
import com.typesafe.config.Config
import javax.inject.{Inject, Singleton}
import scala.PartialFunction.cond
import scala.collection.JavaConversions._
import spray.http.HttpMethods.POST
import spray.http.MediaTypes.{`application/x-www-form-urlencoded`, `multipart/form-data`, `text/plain`}
import spray.http.StatusCodes.Forbidden
import spray.http.{HttpEntity, HttpRequest}
import spray.routing.Directives._
import spray.routing._

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
      requestInstance { request ⇒
        if (looksLikeHtmlFormPost(request))
          completeWithError(Forbidden, "HTML form POST is forbidden")
        else
          inner
      }
    }

  private def looksLikeHtmlFormPost(request: HttpRequest): Boolean =
    request.method == POST &&
      cond(request.entity) {
        case HttpEntity.NonEmpty(contentType, _) ⇒ conf.rejectedContentTypes contains contentType.mediaType.value
      }
}

object CSRF {

  final case class Configuration(
    /** For a list of cross-site HTML 5 form POST content types, see https://www.w3.org/TR/html5/forms.html#attr-fs-enctype. */
    rejectedContentTypes: Set[String])

  object Configuration {
    // See https://www.w3.org/TR/html5/forms.html#attr-fs-enctype
    val Default = Configuration(
      rejectedContentTypes = Set(`application/x-www-form-urlencoded`, `multipart/form-data`, `text/plain`) map { _.value })

    def fromSubConfig(config: Config) = new Configuration(
      rejectedContentTypes =
        if (config.hasPath("reject-post-content-types"))
          config.getStringList("reject-post-content-types").toSet
        else
          Default.rejectedContentTypes)
  }
}
