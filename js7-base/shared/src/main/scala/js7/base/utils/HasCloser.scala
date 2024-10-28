package js7.base.utils

import js7.base.utils.Nulls.isNull

trait HasCloser extends AutoCloseable:
  private val _closer: Closer = new Closer

  protected implicit final def closer: Closer =
    if isNull(_closer) then
      throw new NullPointerException(s"$getClass should extend HasClose further in front?")
    _closer

  /** Registers the function for execution in close(), in reverse order of registering. */
  protected def onClose(f: => Unit): Unit = closer.onClose { f }

  def close(): Unit = closer.close()
