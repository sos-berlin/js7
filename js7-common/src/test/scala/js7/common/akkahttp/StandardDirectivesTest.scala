package js7.common.akkahttp

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import js7.base.generic.GenericString
import js7.common.akkahttp.StandardDirectives._
import js7.data.item.APath
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StandardDirectivesTest extends AnyFreeSpec
{
  private val matched = Matched(Path.Empty, Tuple1(APath("/FOLDER/NAME")))

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

  "remainingTypedPath" - {
    "remainingTypedPath with TypedPath" in {
      assert(remainingTypedPath[APath].apply(Path("PATH")) == Matched(Path.Empty, Tuple1(APath("/PATH"))))
      assert(remainingTypedPath[APath].apply(Path("invalid,character")) == Unmatched)
      assert(remainingTypedPath[APath].apply(Path.Empty)                == Unmatched)
    }

    "Properly encoded TypedPath (percent character encodes hexadecimal bytes)" in {
      // Example: /api/workflow/%2FFOLDER%2FMY-WORKFLOW.
      // If you want to interpret the percent characted as a hexadecimal encoding prefix,
      // then do not omit the first slash but encode it as %2F, as each slash in the TypedPath.
      // This allows to set the TypedPath in a programmable way in a single path segment.
      assert(remainingTypedPath[APath].apply(Path("%2FNAME"))  == Matched(Path.Empty, Tuple1(APath("/NAME"))))
      assert(remainingTypedPath[APath].apply(Path("%2FFOLDER%2FNAME"))  == matched)
      assert(remainingTypedPath[APath].apply(Path("%2FFOLDER%2Fwith%252Fpercent")) == Matched(Path.Empty, Tuple1(APath("/FOLDER/with%2Fpercent"))))
    }

    "Raw TypedPath, use it for comfortable typing" in {
      // Example: /api/workflow/FOLDER/MY-WORKFLOW.
      // Type the first slash of the TypedPath as unencoded '/' if you do not want the percent character to be interpreted.
      // This allows to type the URI in a comfortable way, but this is not recommened for programming.
      // If the first name path does not start with a percent characted start starting slash may be ommitted.

      // Single segment
      assert(remainingTypedPath[APath].apply(Path("with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/with%2Fpercent"))))
      assert(remainingTypedPath[APath].apply(Path("/%2Fpercent"))    == Matched(Path.Empty, Tuple1(APath("/%2Fpercent"))))

      // BUT, WITH STARTING SLASH OMMITTED
      assert(remainingTypedPath[APath].apply(Path("%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/percent"))))

      // Two segments
      assert(remainingTypedPath[APath].apply(Path("FOLDER/NAME"))       == matched)
      assert(remainingTypedPath[APath].apply(Path("FOLDER") / "NAME")   == matched)
      assert(remainingTypedPath[APath].apply(Path("FOLDER/with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/FOLDER/with%2Fpercent"))))
      assert(remainingTypedPath[APath].apply(Path("/FOLDER/with%2Fpercent")) == Matched(Path.Empty, Tuple1(APath("/FOLDER/with%2Fpercent"))))
      assert(remainingTypedPath[APath].apply(Path("/FOLDER/NAME"))      == matched)
      assert(remainingTypedPath[APath].apply(Path.Slash(Path("FOLDER") / "NAME")) == matched)
    }
  }
}
