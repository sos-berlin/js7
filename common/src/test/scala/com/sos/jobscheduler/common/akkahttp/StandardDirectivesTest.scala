package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import com.sos.jobscheduler.common.akkahttp.StandardDirectives._
import com.sos.jobscheduler.data.filebased.APath
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StandardDirectivesTest extends FreeSpec
{
  "remainingSegmentOrPath with TypedPath" in {
    assert(remainingSegmentOrPath[APath].apply(Path("PATH")) == Matched(Path.Empty, Tuple1(APath("/PATH"))))
    assert(remainingSegmentOrPath[APath].apply(Path("FOLDER%2FNAME")) == Matched(Path.Empty, Tuple1(APath("/FOLDER/NAME"))))
    assert(remainingSegmentOrPath[APath].apply(Path("FOLDER") / "NAME") == Matched(Path.Empty, Tuple1(APath("/FOLDER/NAME"))))
    assert(remainingSegmentOrPath[APath].apply(Path("invalid,character")) == Unmatched)
    assert(remainingSegmentOrPath[APath].apply(Path.Empty) == Unmatched)
  }
}
