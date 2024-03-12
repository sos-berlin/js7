package js7.base.utils

final class Once:

  private var once = false
  @volatile private var _isInitializing = false

  def isInitialized: Boolean =
    once

  def isInitializing: Boolean =
    _isInitializing

  def apply(body: => Unit): Unit =
    if !once then
      synchronized:
        if !once then
          _isInitializing = true
          body
          once = true
