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
  "remainingSegmentOrPath with TypedPath" - {
    assert(remainingSegmentOrPath[APath].apply(Path("PATH")) == Matched(Path.Empty, Tuple1(APath("/PATH"))))
    val matched = Matched(Path.Empty, Tuple1(APath("/FOLDER/NAME")))
    assert(remainingSegmentOrPath[APath].apply(Path("%2FFOLDER%2FNAME"))  == matched)     // Single encoded segment
    assert(remainingSegmentOrPath[APath].apply(Path("FOLDER%2FNAME"))     == matched)     // Single encoded segment without starting slash
    assert(remainingSegmentOrPath[APath].apply(Path("FOLDER/NAME"))       == matched)     // Remaining path (multiple segments)
    assert(remainingSegmentOrPath[APath].apply(Path("/FOLDER/NAME"))      == matched)     // Remaining path (multiple segments) with extra starting slash
    assert(remainingSegmentOrPath[APath].apply(Path("FOLDER") / "NAME")   == matched)     // Remaining path (multiple segments)
    assert(remainingSegmentOrPath[APath].apply(Path.Slash((Path("FOLDER") / "NAME"))) == matched)  // Remaining path (multiple segments) with extra starting slash
    assert(remainingSegmentOrPath[APath].apply(Path("invalid,character")) == Unmatched)
    assert(remainingSegmentOrPath[APath].apply(Path("/FOLDER%2FNAME"))    == Unmatched)   // Mixing single encoded segment and remaining URI
    assert(remainingSegmentOrPath[APath].apply(Path("FOLDER/SUB%2FNAME")) == Unmatched)   // Mixing single encoded segment and remaining URI
    assert(remainingSegmentOrPath[APath].apply(Path.Empty)                == Unmatched)
  }
}
