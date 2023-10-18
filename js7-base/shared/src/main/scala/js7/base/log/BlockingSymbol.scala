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
    increment()
    _debugLogged = true

  def onInfo(): Unit =
    increment(minimum = 2)

  def onWarn(): Unit =
    increment(minimum = 2)
    _warnLogged = true

  private def increment(minimum: Int): Unit =
    if _index < minimum then
      _index = minimum
    else
      increment()

  def increment(): Unit =
    if _index < 3 then
      _index += 1

  def warnLogged: Boolean =
    _warnLogged

  def called: Boolean =
    _index > 0

  def logLevel: LogLevel =
    _index match
      case 0 => LogLevel.LogNone
      case 1 => LogLevel.Debug
      case 2 => LogLevel.Info
      case 3 => LogLevel.Warn

  def releasedLogLevel: LogLevel =
    _index match
      case 0 => LogLevel.LogNone
      case 1 => LogLevel.Debug
      case 2 | 3 => LogLevel.Info

  override def toString =
    symbols(_index)


object BlockingSymbol:
  private val symbols = Array("⚪", "🟡", "🟠", "🔴")
