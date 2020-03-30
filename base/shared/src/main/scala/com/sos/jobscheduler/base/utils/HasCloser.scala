package com.sos.jobscheduler.base.utils

trait HasCloser extends AutoCloseable
{
  private val _closer: Closer = new Closer

  protected implicit final def closer: Closer = {
    if (_closer == null) throw new NullPointerException(s"$getClass should extend HasClose further in front?")
    _closer
  }

  /** Registers the function for execution in close(), in reverse order of registering. */
  protected def onClose(f: => Unit): Unit = closer.onClose { f }

  def close(): Unit = closer.close()
}
