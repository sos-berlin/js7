package js7.base.log

import js7.base.log.BlockingSymbol.*

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
    if _index < 3 then
      _index += 1

  def warnLogged: Boolean =
    _warnLogged

  def used: Boolean =
    _index > 0

  def relievedLogLevel: LogLevel =
    if _index < 3 then logLevel else LogLevel.Info

  def logLevel: LogLevel =
    _index match
      case 0 => LogLevel.None
      case 1 => LogLevel.Debug
      case 2 => LogLevel.Info
      case 3 => LogLevel.Warn

  override def toString: String =
    symbol

  def symbol: String =
    symbols(_index)


object BlockingSymbol:
  private val symbols = Array("⚪", "🟡", "🟠", "🔴")
