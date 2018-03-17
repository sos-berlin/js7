package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import akka.http.scaladsl.server.PathMatcher1
import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.CheckedString

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
      def apply(path: Path) = {
        val string = path match {
          case Path.Segment(segment, Path.Empty) ⇒ segment  // Slashes encoded as %2F
          case _ ⇒ path.toString   // Slashes not encoded
        }
        P.checked("/" + string) match {
          case Valid(typedPath) ⇒ Matched(Path.Empty, Tuple1(typedPath))
          case _ ⇒ Unmatched
        }
      }
    }
}
