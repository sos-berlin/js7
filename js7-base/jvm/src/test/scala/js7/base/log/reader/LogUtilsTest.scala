package js7.base.log.reader

import java.nio.file.Paths
import js7.base.log.LogLevel
import js7.base.log.reader.LogUtils.{fileToPrefixAndLogLevel, isOurLogFilename}
import js7.base.test.OurTestSuite

final class LogUtilsTest extends OurTestSuite:

  "isOurLogFilename" in:
    val prefixes = Set("joc", "prefix", "invalid-a", "invalid.a" )
    assert(!isOurLogFilename(prefixes, Paths.get("..")))
    assert(!isOurLogFilename(prefixes, Paths.get("alien.log")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefix-x-2024-06-01-1.log.gz")))
    assert(!isOurLogFilename(prefixes, Paths.get(" prefix.log")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefix.log ")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefixx.log")))
    assert(!isOurLogFilename(prefixes, Paths.get("prefix.log.gzx")))
    assert(!isOurLogFilename(prefixes, Paths.get("invalid-a.log.gzx")))
    assert(!isOurLogFilename(prefixes, Paths.get("invalid.a.log.gzx")))

    assert(isOurLogFilename(prefixes, Paths.get("prefix.log")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-debug.log")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-error.log")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-2026-10-14-99.log.gz-indexed.tmp")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-2026-10-14-99.log.gz")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-debug-2026-10-14-99.log.gz")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-debug-2026-10-14-99.log.gz-indexed.tmp")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-error-2026-10-14-99.log.gz")))
    assert(isOurLogFilename(prefixes, Paths.get("prefix-error-2026-10-14-99.log.gz-indexed.tmp")))

    assert(isOurLogFilename(prefixes, Paths.get("joc.log")))

  "fileToPrefixAndLogLevel" in:
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX,log")) == None)
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX.logx")) == None)
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-Debug.log")) == None)
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-debug.log.gzx")) == None)
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-2026-08-14-0.log.gz-indeXed.tmp")) == None)

    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX.log")) == Some("PREFIX" -> LogLevel.Info))
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-2026-08-14-0.log.gz")) == Some("PREFIX" -> LogLevel.Info))
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-2026-08-14-0.log.gz-indexed.tmp")) == Some("PREFIX" -> LogLevel.Info))
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-error.log")) == Some("PREFIX" -> LogLevel.Error))
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-error-2026-08-14-0.log.gz")) == Some("PREFIX" -> LogLevel.Error))
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-debug.log")) == Some("PREFIX" -> LogLevel.Debug))
    assert(fileToPrefixAndLogLevel(Paths.get("dir/PREFIX-debug-2026-08-14-0.log.gz")) == Some("PREFIX" -> LogLevel.Debug))
