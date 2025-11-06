package js7.tests

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.base.crypt.silly.SillySignature
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.test.OurAsyncTestSuite
import js7.common.commandline.CommandLineArguments
import js7.controller.RunningController
import js7.controller.configuration.ControllerConfiguration
import js7.data.controller.ControllerCommand

final class MinimumConfigurationTest extends OurAsyncTestSuite:

  "Minimum (unsafe) configuration" in:
    temporaryDirectoryResource[IO]("MinimumConfigurationTest-").use: dir =>
      val config = dir / "config"
      val data = dir / "data"
      createRequiredDirectoryAndFiles(config, data)

      given IORuntime = ioRuntime
      RunningController.resource:
        ControllerConfiguration.fromCommandLine:
          CommandLineArguments(Seq(
            "--config-directory=" + config,
            "--data-directory=" + data))
      .use: controller =>
        controller.untilReady *>
          controller.shutdown(ControllerCommand.ShutDown())
            .as(succeed)

  private def createRequiredDirectoryAndFiles(config: Path, data: Path): Unit =
    val privateDir = config / "private"
    val trustedSignatureKeysDir = privateDir / "trusted-silly-signature-keys"

    createDirectory(config)
    createDirectory(privateDir)
    createDirectory(trustedSignatureKeysDir)
    createDirectory(data)
    createDirectory(data / "state")
    createDirectory(data / "work")

    val signature = SillySignature("MY-SILLY-SIGNATURE")
    trustedSignatureKeysDir / "trusted-silly-signature-key.txt" := signature.string
    config / "private" / "private.conf" ++= """
      js7.configuration.trusted-signature-keys.Silly =
        ${js7.config-directory}"/private/trusted-silly-signature-keys"
      """

