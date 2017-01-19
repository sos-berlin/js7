package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.base.convert.As
import com.sos.scheduler.engine.base.utils.ScalaUtils.RichThrowable
import com.sos.scheduler.engine.common.time.ScalaTime.parseDuration
import java.time.Duration
import scala.annotation.tailrec
import scala.util.control.NonFatal
import shapeless.{::, HNil}
import spray.http.HttpHeaders.Accept
import spray.http.{ContentType, HttpHeader, MediaType, StatusCode, Uri}
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.httpx.unmarshalling.{ContentExpected, Deserialized, FromStringOptionDeserializer, MalformedContent}
import spray.routing.Directives._
import spray.routing._

/**
  * @author Joacim Zschimmer
  */
object SprayUtils {
  object implicits {
    implicit class RichOption[A](val delegate: Option[A]) extends AnyVal {
      def applyRoute(f: A ⇒ Route): Route =
        delegate match {
          case Some(a) ⇒ f(a)
          case None ⇒ reject
        }
    }
  }

  def completeWithError(status: StatusCode, message: String) =
    mapRequestContext(_.withContentNegotiationDisabled) {
      complete(isMarshallable(status → message))
    }


  def accept(mediaType: MediaType, mediaTypes: MediaType*): Directive0 =
    accept(mediaTypes.toSet + mediaType)

  def accept(mediaTypes: Iterable[MediaType]): Directive0 =
    mapInnerRoute { route ⇒
      headerValueByType[Accept]() {
        case Accept(requestedMediaTypes) if requestedMediaTypes exists { o ⇒ mediaTypes exists o.matches } ⇒
          route
        case _ ⇒
          reject(UnacceptedResponseContentTypeRejection((mediaTypes map { m ⇒ ContentType(m) }).toList))
      }
    }

  /**
    * Passes x iff argument is Some(x).
    */
  def passSome[A](option: Option[A]): Directive1[A] = {
    import shapeless.::
    new Directive1[A] {
      def happly(inner: (A :: HNil) ⇒ Route) =
        option match {
          case Some(o) ⇒ inner(o :: HNil)
          case None ⇒ reject
        }
    }
  }

  def passRight[R](either: Either[String, R]): Directive1[R] =
    new Directive1[R] {
      def happly(inner: (R :: HNil) ⇒ Route) =
        either match {
          case Right(r) ⇒ inner(r :: HNil)
          case Left(message) ⇒ reject(ValidationRejection(message))
        }
    }

  /**
    * Passes x iff argument is true.
    */
  def passIf(condition: Boolean): Directive0 =
    mapInnerRoute { inner ⇒
      if (condition)
        inner
      else
        reject
    }

  def addHeader(header: HttpHeader): Directive0 =
    mapRequest(o ⇒ o.copy(headers = header :: o.headers))

  /*
  private type ParameterMap = Map[String, String]

  private def eatParameterOption(parameterMap: ParameterMap, key: String) =
    new Directive[ParameterMap :: Option[String] :: HNil] {
      override def happly(inner: (ParameterMap :: Option[String] :: HNil) ⇒ Route) =
        inner((parameterMap - key) :: parameterMap.get(key) :: HNil)
    }

  private def eatParameter(parameterMap: ParameterMap, key: String) =
    new Directive[ParameterMap :: String :: HNil] {
      override def happly(inner: (ParameterMap :: String :: HNil) ⇒ Route) =
        parameterMap.get(key) match {
          case Some(value) ⇒ inner((parameterMap - key) :: value :: HNil)
          case None ⇒ reject
        }
    }

  def removeParameters(keys: Set[String]): Directive0 =
    mapRequest { request ⇒
      request.copy(uri = request.uri.copy(query = removeKeysFromQuery(keys, request.uri.query)))
    }

  private def removeKeysFromQuery(keys: Set[String], query: Uri.Query): Uri.Query = {
    query match {
      case Uri.Query.Empty ⇒ Uri.Query.Empty
      case q @ Uri.Query.Cons(key, value, tail, keep) ⇒
        if (keys contains key)
          removeKeysFromQuery(keys, tail)
        else
          Uri.Query.Cons(key, value, removeKeysFromQuery(keys, tail), keep)
      case q: Uri.Query.Raw ⇒ q
    }
  }

  def noParameters(keys: Set[String]): Directive0 =
    mapInnerRoute { inner ⇒
      requestUri { uri ⇒
        if (uri.query.isEmpty)
          inner
        else
          reject(ValidationRejection(s"Invalid parameters: ${keys mkString ", "}"))
      }
    }
*/

  def emptyParameterMap(parameterMap: Map[String, String]) =
    mapInnerRoute { route ⇒
      if (parameterMap.isEmpty) route
      else reject(ValidationRejection(s"Invalid parameters: ${parameterMap.keys mkString ", "}"))
    }

  implicit def asFromStringOptionDeserializer[A](implicit stringAsA: As[String, A]) =
    simpleFromStringOptionDeserializer(stringAsA)

//  implicit def isStringFromStringOptionDeserializer[A <: IsString: IsString.Companion]: FromStringOptionDeserializer[A] =
//    simpleFromStringOptionDeserializer(implicitly[IsString.Companion[A]].apply)

  implicit val DurationFromStringOptionDeserializer: FromStringOptionDeserializer[Duration] =
    simpleFromStringOptionDeserializer(parseDuration)

  final def simpleFromStringOptionDeserializer[A](fromString: String ⇒ A) =
    new FromStringOptionDeserializer[A] {
      def apply(value: Option[String]): Deserialized[A] =
        value match {
          case Some(string) ⇒
            try Right(fromString(string))
            catch {
              case NonFatal(t) ⇒ Left(new MalformedContent(t.toSimplifiedString, Some(t)))
            }
          case None ⇒
            Left(ContentExpected)
        }
    }

  implicit class RichPath(val delegate: Uri.Path) extends AnyVal {
    import Uri.Path._

    /**
      * Matches complete segments (not characters, as `startWith`).
      */
    @tailrec
    final def startsWithPath(prefix: Uri.Path): Boolean =
      (delegate, prefix) match {
        case (Slash(a), Slash(b)) ⇒ a startsWithPath b
        case (Segment(aHead, aTail), Segment(bHead, bTail)) ⇒ aHead == bHead && (aTail startsWithPath bTail)
        case _ ⇒ prefix.isEmpty
      }

    def drop(n: Int): Uri.Path =
      if (n == 0)
        delegate
      else {
        require(n > 0)
        delegate.tail drop n - 1
      }
  }

  /**
    * Like `pathPrefix`, but `prefix` denotes a path of complete path segments.
    */
  def pathSegments(prefix: String): Directive0 =
    pathSegments(Uri.Path(prefix))

  /**
    * Like `pathPrefix`, but matches complete path segments.
    */
  def pathSegments(prefix: Uri.Path): Directive0 =
    pathPrefix(matchSegments(prefix))

  /**
    * A `PathMatcher` for Directive `pathPrefix` matching complete segments.
    */
  def matchSegments(prefix: Uri.Path): PathMatcher0 = {
    import PathMatcher._
    if (prefix.isEmpty)
      provide(HNil)
    else
      new PathMatcher[HNil] {
        def apply(path: Uri.Path) =
          if (path startsWithPath prefix)
            Matched(path drop prefix.length, HNil)
          else
            Unmatched
        }
  }
}
