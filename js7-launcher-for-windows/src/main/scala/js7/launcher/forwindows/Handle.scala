package js7.launcher.forwindows

import com.sun.jna.platform.win32.Kernel32
import com.sun.jna.platform.win32.WinBase.INVALID_HANDLE_VALUE
import com.sun.jna.platform.win32.WinNT.HANDLE
import js7.base.log.Logger
import js7.base.utils.Atomic
import js7.launcher.forwindows.Handle.*
import js7.launcher.forwindows.WindowsApi.{WindowsException, call}

final class Handle(private var handleVar: HANDLE) extends AutoCloseable:

  private val pipeClosed = Atomic(false)

  def handle: HANDLE =
    handleVar

  inline def isValid: Boolean =
    !isInvalid

  inline def isInvalid: Boolean =
    handleVar != INVALID_HANDLE_VALUE

  /** Closes and destroys the handle, does not throw. */
  def close(): Unit =
    val h = handleVar
    if !pipeClosed.getAndSet(true) && h != INVALID_HANDLE_VALUE then
      handleVar = INVALID_HANDLE_VALUE
      try
        call("CloseHandle"):
          Kernel32.INSTANCE.CloseHandle(h)
      catch case e: WindowsException =>
        logger.error(s"Windows API CloseHandle: $e")


object Handle:
  private val logger = Logger[Handle.type]

  val invalid: Handle =
    new Handle(INVALID_HANDLE_VALUE)
