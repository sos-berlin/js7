package js7.executor.process

import java.nio.file.Files.exists
import js7.base.io.file.FileUtils._
import js7.base.io.file.FileUtils.syntax._
import js7.data.value.{NamedValues, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class ShellReturnValuesProviderTest extends AnyFreeSpec
{
  "ShellReturnValuesProvider" in {
    withTemporaryDirectory("ShellReturnValuesProviderTest-") { dir =>
      val provider = new ShellReturnValuesProvider(dir)
      assert(provider.toEnv == "JS7_RETURN_VALUES" -> provider.file.toString)
      provider.file :=
        """A=a
          |B=b * *
          |C=c = =
          |""".stripMargin
      assert(provider.read() == NamedValues(
        "A" -> StringValue("a"),
        "B" -> StringValue("b * *"),
        "C" -> StringValue("c = =")))
      provider.tryDeleteFile()
      assert(!exists(provider.file))
    }
  }

  "v1Compatible=true" in {
    withTemporaryDirectory("ShellReturnValuesProviderTest-") { dir =>
      val provider = new ShellReturnValuesProvider(dir, v1Compatible = true)
      assert(provider.toEnv == "SCHEDULER_RETURN_VALUES" -> provider.file.toString)
    }
  }
}
