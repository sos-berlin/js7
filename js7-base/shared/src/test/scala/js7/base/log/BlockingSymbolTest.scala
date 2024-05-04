package js7.base.log

import js7.base.test.OurTestSuite

final class BlockingSymbolTest extends OurTestSuite:

  "test" in:
    val sym = new BlockingSymbol
    assert(sym.logLevel == LogLevel.None)
    assert(sym.relievedLogLevel == LogLevel.None)

    sym.onDebug()
    assert(sym.logLevel == LogLevel.Debug)
    assert(sym.relievedLogLevel == LogLevel.Debug)

    sym.onDebug()
    assert(sym.logLevel == LogLevel.Debug)
    assert(sym.relievedLogLevel == LogLevel.Debug)

    sym.onInfo()
    assert(sym.logLevel == LogLevel.Info)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.onInfo()
    assert(sym.logLevel == LogLevel.Info)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.onWarn()
    assert(sym.logLevel == LogLevel.Warn)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.onWarn()
    assert(sym.logLevel == LogLevel.Warn)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.clear()
    assert(sym.logLevel == LogLevel.None)
    assert(sym.relievedLogLevel == LogLevel.None)

    sym.escalate()
    assert(sym.logLevel == LogLevel.Debug)
    assert(sym.relievedLogLevel == LogLevel.Debug)

    sym.escalate()
    assert(sym.logLevel == LogLevel.Info)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.escalate()
    assert(sym.logLevel == LogLevel.Warn)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.escalate()
    assert(sym.logLevel == LogLevel.Warn)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.clear()

    sym.onWarn()
    assert(sym.logLevel == LogLevel.Warn)
    assert(sym.relievedLogLevel == LogLevel.Info)

    sym.onWarn()
    assert(sym.logLevel == LogLevel.Warn)
    assert(sym.relievedLogLevel == LogLevel.Info)
