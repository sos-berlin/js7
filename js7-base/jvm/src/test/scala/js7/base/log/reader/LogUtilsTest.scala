package js7.base.log.reader

import java.nio.file.Paths
import js7.base.log.LogLevel
import js7.base.log.reader.LogUtils.fileToPrefixAndLogLevel
import js7.base.test.OurTestSuite

final class LogUtilsTest extends OurTestSuite:

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
