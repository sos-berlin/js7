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
  private val matched = Matched(Path.Empty, Tuple1(APath("/FOLDER/NAME")))

  "remainingSegmentOrPath" - {
    "remainingSegmentOrPath with TypedPath" in {
      assert(remainingSegmentOrPath[APath].apply(Path("PATH")) == Matched(Path.Empty, Tuple1(APath("/PATH"))))
      assert(remainingSegmentOrPath[APath].apply(Path("invalid,character")) == Unmatched)
      assert(remainingSegmentOrPath[APath].apply(Path.Empty)                == Unmatched)
    }

    "Properly encoded TypedPath (percent character encodes hexadecimal bytes)" in {
      // Example: /api/workflow/%2FFOLDER%2FMY-WORKFLOW.
      // If you want to interpret the percent characted as a hexadecimal encoding prefix,
      // then do not omit the first slash but encode it as %2F, as each slash in the TypedPath.
      // This allows to set the TypedPath in a programmable way in a single path segment.
      assert(remainingSegmentOrPath[APath].apply(Path("%2FNAME"))  == Matched(Path.Empty, Tuple1(APath("/NAME"))))
      assert(remainingSegmentOrPath[APath].apply(Path("%2FFOLDER%2FNAME"))  == matched)
      assert(remainingSegmentOrPath[APath].apply(Path("%2FFOLDER%2Fwith%252Fpercent")) == Matched(Path.Empty, Tuple1(APath("/FOLDER/with%2Fpercent"))))
    }

    "Raw TypedPath, use it for comfortable typing" in {
      // Example: /api/workflow/FOLDER/MY-WORKFLOW.
      // Type the first slash of the TypedPath as unencoded '/' if you do not want the percent character to be interpreted.
      // This allows to type the URI in a comfortable way, but this is not recommened for programming.
      // If the first name path does not start with a percent characted start starting slash may be ommitted.

      // Single segment
      assert(remainingSegmentOrPath[APath].apply(Path("with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/with%2Fpercent"))))
      assert(remainingSegmentOrPath[APath].apply(Path("/%2Fpercent"))    == Matched(Path.Empty, Tuple1(APath("/%2Fpercent"))))

      // BUT, WITH STARTING SLASH OMMITTED
      assert(remainingSegmentOrPath[APath].apply(Path("%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/percent"))))

      // Two segments
      assert(remainingSegmentOrPath[APath].apply(Path("FOLDER/NAME"))       == matched)
      assert(remainingSegmentOrPath[APath].apply(Path("FOLDER") / "NAME")   == matched)
      assert(remainingSegmentOrPath[APath].apply(Path("FOLDER/with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/FOLDER/with%2Fpercent"))))
      assert(remainingSegmentOrPath[APath].apply(Path("/FOLDER/with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/FOLDER/with%2Fpercent"))))
      assert(remainingSegmentOrPath[APath].apply(Path("/FOLDER/NAME"))      == matched)
      assert(remainingSegmentOrPath[APath].apply(Path.Slash(Path("FOLDER") / "NAME")) == matched)
    }
  }
}
