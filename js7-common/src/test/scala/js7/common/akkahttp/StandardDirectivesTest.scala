package js7.common.akkahttp

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import js7.base.generic.GenericString
import js7.base.test.OurTestSuite
import js7.common.akkahttp.StandardDirectives.*
import js7.data.item.APath

/**
  * @author Joacim Zschimmer
  */
final class StandardDirectivesTest extends OurTestSuite
{
  private val matched = Matched(Path.Empty, Tuple1(APath("FOLDER/NAME")))

  "remainingPath" in {
    case class A(string: String) extends GenericString
    object A extends GenericString.NonEmpty[A] {
      protected def unchecked(string: String) = new A(string)
    }

    assert(remainingPath[A].apply(Path("PATH")) == Matched(Path.Empty, Tuple1(A("PATH"))))
    // % is not interpreted (despite RFC ?)
    assert(remainingPath[A].apply(Path("PATH%2FX")) == Matched(Path.Empty, Tuple1(A("PATH%2FX"))))
    assert(remainingPath[A].apply(Path("PATH/X")) == Matched(Path.Empty, Tuple1(A("PATH/X"))))
    assert(remainingPath[A].apply(Path.Empty) == Unmatched)
  }

  "remainingItemPath" - {
    "remainingItemPath with VersionedItemPath" in {
      assert(remainingItemPath[APath].apply(Path("PATH")) == Matched(Path.Empty, Tuple1(APath("PATH"))))
      assert(remainingItemPath[APath].apply(Path("invalid,character")) == Unmatched)
      assert(remainingItemPath[APath].apply(Path.Empty)                == Unmatched)
    }

    "Properly encoded VersionedItemPath (percent character encodes hexadecimal bytes)" in {
      // Example: /api/workflow/%2FFOLDER%2FMY-WORKFLOW.
      // If you want to interpret the percent characted as a hexadecimal encoding prefix,
      // then do not omit the first slash but encode it as %2F, as each slash in the VersionedItemPath.
      // This allows to set the VersionedItemPath in a programmable way in a single path segment.
      assert(remainingItemPath[APath].apply(Path("NAME"))  == Matched(Path.Empty, Tuple1(APath("NAME"))))
      assert(remainingItemPath[APath].apply(Path("FOLDER%2FNAME"))  == matched)
      //assert(remainingItemPath[APath].apply(Path("FOLDER%2Fwith%252Fpercent")) == Matched(Path.Empty, Tuple1(APath("FOLDER/with%2Fpercent"))))
    }

    "Raw VersionedItemPath, use it for comfortable typing" in {
      // Example: /api/workflow/FOLDER/MY-WORKFLOW.
      // Type the first slash of the VersionedItemPath as unencoded '/' if you do not want the percent character to be interpreted.
      // This allows to type the URI in a comfortable way, but this is not recommened for programming.
      // If the first name path does not start with a percent characted start starting slash may be ommitted.

      // Single segment
      //assert(remainingItemPath[APath].apply(Path("with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("with%2Fpercent"))))
      //assert(remainingItemPath[APath].apply(Path("/%2Fpercent"))    == Matched(Path.Empty, Tuple1(APath("%2Fpercent"))))

      // BUT, WITH STARTING SLASH OMMITTED
      //assert(remainingItemPath[APath].apply(Path("%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("percent"))))

      // Two segments
      assert(remainingItemPath[APath].apply(Path("FOLDER/NAME"))       == matched)
      assert(remainingItemPath[APath].apply(Path("FOLDER") / "NAME")   == matched)
      //assert(remainingItemPath[APath].apply(Path("FOLDER/with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("FOLDER/with%2Fpercent"))))
      //assert(remainingItemPath[APath].apply(Path("/FOLDER/with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("FOLDER/with%2Fpercent"))))
      assert(remainingItemPath[APath].apply(Path("/FOLDER/NAME"))      == Unmatched)
      assert(remainingItemPath[APath].apply(Path.Slash(Path("FOLDER") / "NAME")) == Unmatched)
    }
  }
}
