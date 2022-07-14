package js7.launcher.process

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.exists
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.data.value.{NamedValues, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class ShellReturnValuesProviderTest extends AnyFreeSpec
{
  "ShellReturnValuesProvider" in {
    withTemporaryDirectory("ShellReturnValuesProviderTest-") { dir =>
      val provider = new ShellReturnValuesProvider(dir, UTF_8)
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
      val provider = new ShellReturnValuesProvider(dir, UTF_8, v1Compatible = true)
      assert(provider.toEnv == "SCHEDULER_RETURN_VALUES" -> provider.file.toString)
    }
  }
}
