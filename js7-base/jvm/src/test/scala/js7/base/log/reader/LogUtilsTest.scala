package js7.base.log.reader

import java.nio.file.Paths
import js7.base.log.LogLevel
import js7.base.log.reader.LogUtils.{fileToPrefixAndLogLevel, isOurLogFilename}
import js7.base.test.OurTestSuite

final class LogUtilsTest extends OurTestSuite:

  "isOurLogFilename" in:
    val prefixes = Set("joc", "prefix", "prefix-two", "prefix-errors", "invalid.a" )
    assert(!isOurLogFilename(prefixes, Paths.get("..")))
    assert(!isOurLogFilename(prefixes, Paths.get("alien.log")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefix-x-2024-06-01-1.log.gz")))
    assert(!isOurLogFilename(prefixes, Paths.get(" prefix.log")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefix.log ")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefixx.log")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefix.log.gzx")))
    assert(!isOurLogFilename(prefixes, Paths.get("invalid.a.log.gzx")))

    for prefix <- Array("prefix", "prefix-two", "prefix-errors") do withClue(s"prefix=$prefix: "):
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix.log")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-debug.log")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-error.log")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-2026-10-14-99.log.gz")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-2026-10-14-99.log.gz-indexed.tmp")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-debug-2026-10-14.log.gz")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-debug-2026-10-14-99.log.gz")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-debug-2026-10-14-99.log.gz-indexed.tmp")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-error-2026-10-14-99.log.gz")))
      assert(isOurLogFilename(prefixes, Paths.get(s"$prefix-error-2026-10-14-99.log.gz-indexed.tmp")))

    assert(isOurLogFilename(prefixes, Paths.get("joc.log")))

  "fileToPrefixAndLogLevel" in:
    for prefix <- Array("PREFIX", "PREFIX-TWO") do withClue(s"prefix=$prefix: "):
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix,log")) == None)
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix.logx")) == None)
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-Debug.log")) == None)
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-debug.log.gzx")) == None)
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-2026-08-14-0.log.gz-indeXed.tmp")) == None)

      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix.log")) == Some(s"$prefix" -> LogLevel.Info))
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-2026-08-14-0.log.gz")) == Some(s"$prefix" -> LogLevel.Info))
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-2026-08-14-0.log.gz-indexed.tmp")) == Some(s"$prefix" -> LogLevel.Info))
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-error.log")) == Some(s"$prefix" -> LogLevel.Error))
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-error-2026-08-14-0.log.gz")) == Some(s"$prefix" -> LogLevel.Error))
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-debug.log")) == Some(s"$prefix" -> LogLevel.Debug))
      assert(fileToPrefixAndLogLevel(Paths.get(s"dir/$prefix-debug-2026-08-14-0.log.gz")) == Some(s"$prefix" -> LogLevel.Debug))
