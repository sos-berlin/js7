package js7.common.akkahttp.html

import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.MediaTypes.`text/html`
import akka.http.scaladsl.model.StatusCodes.TemporaryRedirect
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import akka.http.scaladsl.model.{HttpRequest, MediaRange, Uri}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{Directive0, RejectionHandler, Route}
import js7.common.akkahttp.AkkaHttpServerUtils.passIf

/**
  * @author Joacim Zschimmer
  */
object HtmlDirectives:
  def dontCache: Directive0 =
    mapInnerRoute { inner =>
      extractRequest { request =>
        val header =
          if isHtmlPreferred(request) then
            `Cache-Control`(`max-age`(0))  // This allows browsers to use the cache when hitting the back button - for good user experience
          else
            `Cache-Control`(`max-age`(0), `no-store`, `no-cache`)
        respondWithHeader(header):
          inner
      }
    }

  /**
    * If HTML is requested, path ends with slash and request has no query, then redirect to path without slash, in case of typo.
    */
  val pathEndRedirectToSlash: Route =
    pathEnd:
      redirectEmptyQueryBy(path => Uri.Path(path.toString + "/"))

  /**
    * If HTML is requested, path ends with slash and request has no query, then redirect to path without slash, in case of typo.
    */
  def pathEndElseRedirect: Directive0 =
    mapInnerRoute { route =>
      pathEnd {
        route
      } ~
      pathSingleSlash:
        htmlPreferred:
          get:
            extractRequest { request =>
              passIf(request.uri.query() == Uri.Query.Empty):
                val withoutSlash = request.uri.copy(
                  scheme = "",
                  authority = Uri.Authority.Empty,
                  path = Uri.Path(request.uri.path.toString stripSuffix "/"))
                redirect(withoutSlash, TemporaryRedirect)
            }
    }

  /**
    * If HTML is requested, trailing slash is missing and request has no query, then redirect to trailing slash, in case of typo.
    */
  def getRequiresSlash: Directive0 =
    mapInnerRoute { route =>
      get {
        redirectToSlash ~
        extractUnmatchedPath:
          case _: Uri.Path.Slash => route
          case _ => reject
      } ~
        route
    }

  /**
    * If HTML is requested, trailing slash is missing and request has no query, then redirect to trailing slash, in case of typo.
    */
  val redirectToSlash: Route =
    pathEnd:
      htmlPreferred:  // The browser user may type "api/"
        extractRequest { request =>
          passIf(request.uri.query() == Uri.Query.Empty):
            val withSlash = request.uri.copy(
              scheme = "",
              authority = Uri.Authority.Empty,
              path = Uri.Path(request.uri.path.toString + "/"))
            redirect(withSlash, TemporaryRedirect)
        }

  /**
    * If HTML is requested and request has no query, then redirect according to `changePath`, in case of user typo.
    */
  def redirectEmptyQueryBy(changePath: Uri.Path => Uri.Path): Route =
    htmlPreferred:
      get:
        extractRequest { request =>
          if request.uri.query() == Uri.Query.Empty then
            redirect(
              request.uri.copy(
                scheme = "",
                authority = Uri.Authority.Empty,
                path = changePath(request.uri.path)),
              TemporaryRedirect)
          else
            reject
        }

  def htmlPreferred: Directive0 =
    mapInnerRoute { route =>
      extractRequest { request =>
        passIf(request.method == GET && isHtmlPreferred(request)):
          handleRejections(RejectionHandler.default):
            route
      }
    }

  private def isHtmlPreferred(request: HttpRequest): Boolean =
    request.header[Accept] exists { o => isHtmlPreferred(o.mediaRanges) }

  /**
    * Workaround for Spray 1.3.3, which weights the MediaType ordering of the UnMarshaller over the (higher) weight of more specific MediaRange.
    * <p>
    * <a href="https://tools.ietf.org/html/rfc7231#section-5.3.2">https://tools.ietf.org/html/rfc7231#section-5.3.2</a>.
    */
  private def isHtmlPreferred(mediaRanges: Iterable[MediaRange]): Boolean =
    mediaRanges exists:
      case MediaRange.One(`text/html`, 1.0f) => true  // Highest priority q < 1 is not respected (and should be unusual for a browser)
      case _ => false
