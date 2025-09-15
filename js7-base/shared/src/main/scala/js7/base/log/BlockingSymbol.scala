package js7.base.log

import js7.base.log.BlockingSymbol.*
import js7.base.log.LogLevel.Warn

/** Escalating symbols ⚪🟡🟠🔴 to show the patient retries after a temporary blocking failure.*/
final class BlockingSymbol:

  private var _index = 0
  private var _debugLogged = false
  private var _warnLogged = false

  def clear(): Unit =
    _index = 0
    _debugLogged = false
    _warnLogged = false

  def onDebug(): Unit =
    _index = _index max 1
    _debugLogged = true

  def onInfo(): Unit =
    _index = _index max 2

  def onWarn(): Unit =
    _index = _index max 3
    _warnLogged = true

  def escalate(): Unit =
    escalateUpTo(3)

  def escalate(minLevel: LogLevel, maxLevel: LogLevel = Warn): Unit =
    val min = logLevelToIndex(minLevel)
    val max = logLevelToIndex(maxLevel)
    _index = (_index + 1).max(min).min(max)

  def escalateUpTo(limit: Int): Unit =
    if _index < limit then
      _index += 1

  def warnLogged: Boolean =
    _warnLogged

  def used: Boolean =
    _index > 0

  def relievedLogLevel: LogLevel =
    if _index < 3 then logLevel else LogLevel.Info

  def logLevel: LogLevel =
    indexToLogLevel(_index)

  override def toString: String =
    symbol

  def symbol: String =
    symbols(_index)


object BlockingSymbol:
  private val symbols = Array("⚪", "🟡", "🟠", "🔴")

  private def indexToLogLevel(index: Int): LogLevel =
    index match
      case 0 => LogLevel.None
      case 1 => LogLevel.Debug
      case 2 => LogLevel.Info
      case 3 => LogLevel.Warn

  private def logLevelToIndex(level: LogLevel): Int =
    level match
      case LogLevel.None => 0
      case LogLevel.Trace => 0
      case LogLevel.Debug => 1
      case LogLevel.Info => 2
      case LogLevel.Warn => 3
      case LogLevel.Error => 3
