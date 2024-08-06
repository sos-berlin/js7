package js7.base.utils

final class Once:

  private val blockingLock = BlockingLock()
  private var _isInitialized = false
  @volatile private var _isInitializing = false

  def isInitialized: Boolean =
    _isInitialized

  def isInitializing: Boolean =
    _isInitializing

  def apply(body: => Unit): Unit =
    if !_isInitialized then
      blockingLock.lock:
        if !_isInitialized then
          _isInitializing = true
          try body
          finally _isInitializing = false
          _isInitialized = true
