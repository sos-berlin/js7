package js7.launcher.process

import cats.effect.IO
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurAsyncTestSuite
import js7.data.value.{NamedValues, StringValue}

final class ShellReturnValuesProviderTest extends OurAsyncTestSuite:

  "ShellReturnValuesProvider" in:
    temporaryDirectoryResource[IO]("ShellReturnValuesProviderTest-").use: dir =>
      ShellReturnValuesProvider.resource(dir, UTF_8).use: provider =>
        IO.defer:
          assert(provider.toEnv == "JS7_RETURN_VALUES" -> provider.file.toString)
          provider.file :=
            """A=a
              |B=b * *
              |C=c = =
              |""".stripMargin
          provider.read.map: namedValues =>
            assert(namedValues == NamedValues(
              "A" -> StringValue("a"),
              "B" -> StringValue("b * *"),
              "C" -> StringValue("c = =")))
      .productR:
        IO:
          assert(Files.list(dir).toList.isEmpty)

  "v1Compatible=true" in:
    temporaryDirectoryResource[IO]("ShellReturnValuesProviderTest-").use: dir =>
      ShellReturnValuesProvider.resource(dir, UTF_8, v1Compatible = true).use: provider =>
        IO:
          assert(provider.toEnv == "SCHEDULER_RETURN_VALUES" -> provider.file.toString)
