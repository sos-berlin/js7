package js7.launcher.forwindows

import js7.launcher.forwindows.WindowsCommandLineConversion.argsToCommandLine
import org.scalatest.freespec.AnyFreeSpec

final class WindowsCommandLineConversionTest extends AnyFreeSpec
{
  "argsToCommandLine" in {
    assert(argsToCommandLine(Seq("""a"b""")).isLeft)
    assert(argsToCommandLine(Seq("a", """a"""")).isLeft)

    assert(argsToCommandLine(Seq("./a", "b c")) == Right(""".\a "b c""""))
    assert(argsToCommandLine(Seq("""C:\Program Files\x""", "b c")) ==
      Right(""""C:\Program Files\x" "b c""""))
  }
}
