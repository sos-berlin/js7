package js7.base.log

import js7.base.log.BlockingSymbol.*

/** Escalating symbols ⚪🟡🟠🔴 to show the patient retrys after a temporary blocking failure.*/
final class BlockingSymbol {
  private var _index = 0
  private var _called = false
  private var _debugLogged = false
  private var _infoLogged = false
  private var _warnLogged = false

  def clear(): Unit = {
    _index = 0
    _called = false
    _debugLogged = false
    _infoLogged = false
    _warnLogged = false
  }

  def onDebug(): Unit = {
    increment()
    _debugLogged = true
    _called = true
  }

  def onInfo(): Unit = {
    increment(minimum = 2)
    _infoLogged = true
    _called = true
  }

  def onWarn(): Unit = {
    increment(minimum = 2)
    _warnLogged = true
    _called = true
  }

  private def increment(minimum: Int): Unit = {
    if (_index < minimum)
      _index = minimum
    else
      increment()
  }

  def increment(): Unit =
    if (_index < 3) {
      _index += 1
    }

  def warnLogged: Boolean =
    _warnLogged

  def called: Boolean =
    _called

  def logLevel: LogLevel =
    _index match {
      case 0 => LogLevel.LogNone
      case 1 => LogLevel.Debug
      case 2 => LogLevel.Info
      case 3 => LogLevel.Warn
    }

  def releasedLogLevel: LogLevel =
    _index match {
      case 0 => LogLevel.LogNone
      case 1 => LogLevel.Debug
      case 2 | 3 => LogLevel.Info
    }

  override def toString =
    symbols(_index)
}

object BlockingSymbol {
  private val symbols = Array("⚪", "🟡", "🟠", "🔴")
}
