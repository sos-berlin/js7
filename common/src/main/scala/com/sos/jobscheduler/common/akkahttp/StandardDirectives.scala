package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import akka.http.scaladsl.server.PathMatcher1
import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.{Checked, CheckedString}

/**
  * @author Joacim Zschimmer
  */
object StandardDirectives {
  /**
    * A PathMatcher that matches the last single segment or the whole remaining path,
    * treating encoded slashes (%2F) like unencoded ones.
    * "a/b" ~ "a%2Fb"
    */
  def remainingSegmentOrPath[P](implicit P: CheckedString[P]): PathMatcher1[P] =
    new PathMatcher1[P] {
      def apply(path: Path) =
        uriPathToTypedPath[P](path) match {
          case Valid(typedPath) ⇒ Matched(Path.Empty, Tuple1(typedPath))
          case _ ⇒ Unmatched
        }
    }

  private def uriPathToTypedPath[P](uriPath: Path)(implicit P: CheckedString[P]): Checked[P] =
    uriPath match {
      case Path.Segment(segment, Path.Empty) ⇒
        P.checked("/" + segment.stripPrefix("/"))  // Slashes encoded as %2F; First slash is optional
      case _ ⇒
        P.checked("/" + uriPath)   // Slashes not encoded
    }
}
