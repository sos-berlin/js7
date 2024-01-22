package js7.launcher.forwindows

import com.sun.jna.platform.win32.Kernel32Util.closeHandle
import com.sun.jna.platform.win32.WinBase.{HANDLE_FLAG_INHERIT, INVALID_HANDLE_VALUE, SECURITY_ATTRIBUTES}
import com.sun.jna.platform.win32.WinNT.{CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY, FILE_SHARE_READ, GENERIC_WRITE, HANDLE, HANDLEByReference}
import java.io.File
import js7.base.utils.Atomic
import js7.launcher.forwindows.WindowsApi.{call, kernel32, throwLastError}

private final class Redirection(
  val startupInfoHandle: HANDLE,
  closeStartupInfoHandle: Boolean,
  val pipeHandle: HANDLE):

  private val released = Atomic(false)
  //private val finished = Atomic(false)
  private val pipeClosed = Atomic(false)

  def releaseStartupInfoHandle(): Unit =
    if !released.getAndSet(true) then
      if closeStartupInfoHandle then
        closeHandle(startupInfoHandle)

  ///**
  //  * Closes the pipe (if opened) and await the completion of PipeToFileThread (if started).
  //  */
  //def finish(): Unit =
  //  if (!finished.getAndSet(true)) {
  //    closePipe()
  //  }

  def closePipe(): Unit =
    if !pipeClosed.getAndSet(true) && pipeHandle != INVALID_HANDLE_VALUE then
      closeHandle(pipeHandle)

private object Redirection:
  private val BufferSize = 4096

  def newStdinPipeRedirection(): Redirection =
    val readRef, writeRef = new HANDLEByReference
    val security = new SECURITY_ATTRIBUTES
    security.bInheritHandle = true
    call("CreatePipe"):
      kernel32.CreatePipe(readRef, writeRef, security, BufferSize)
    call("SetHandleInformation"):
      kernel32.SetHandleInformation(writeRef.getValue, HANDLE_FLAG_INHERIT, 0)
    new Redirection(readRef.getValue, true, writeRef.getValue)

  def newStdouterrPipeRedirection(): Redirection =
    val readRef, writeRef = new HANDLEByReference
    val security = new SECURITY_ATTRIBUTES
    security.bInheritHandle = true
    call("CreatePipe"):
      kernel32.CreatePipe(readRef, writeRef, security, BufferSize)
    call("SetHandleInformation"):
      kernel32.SetHandleInformation(readRef.getValue, HANDLE_FLAG_INHERIT, 0)
    new Redirection(writeRef.getValue, true, readRef.getValue)

  def forDirectFile(file: File): Redirection =
    val security = new SECURITY_ATTRIBUTES
    security.bInheritHandle = true
    val handle = kernel32.CreateFile(
      file.toString,
      GENERIC_WRITE,
      FILE_SHARE_READ,
      security,
      CREATE_ALWAYS,
      FILE_ATTRIBUTE_TEMPORARY,
      null)
    if handle == INVALID_HANDLE_VALUE then throwLastError(s"CreateFile '$file'")
    new Redirection(handle, true, INVALID_HANDLE_VALUE)
