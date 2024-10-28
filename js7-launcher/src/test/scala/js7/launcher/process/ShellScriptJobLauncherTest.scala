package js7.launcher.process

import java.nio.charset.StandardCharsets.ISO_8859_1
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.*

final class ShellScriptJobLauncherTest extends OurTestSuite:
  "writeScriptToFile" in:
    withTemporaryDirectory("ShellScriptJobLauncherTest-") { dir =>
      val script = "råd 1\nråd 2\r\nråd 3\nråd 4\r..."
      val file = ShellScriptJobLauncher
        .writeScriptToFile(script, dir, ISO_8859_1, None, isWindows = false)
        .orThrow
      assert(file.contentString(ISO_8859_1) == script)
      assert(file.toString.endsWith(".sh"))

      val windowsFile = ShellScriptJobLauncher
        .writeScriptToFile(script, dir, ISO_8859_1, None, isWindows = true)
        .orThrow
      assert(windowsFile.contentString(ISO_8859_1) == s"råd 1\r\nråd 2\r\nråd 3\r\nråd 4\r...")
      assert(windowsFile.toString.endsWith(".cmd"))
    }
