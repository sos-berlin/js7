package js7.launcher.forwindows

import com.sun.jna.platform.win32.WinNT.HANDLE
import com.sun.jna.ptr.IntByReference
import java.io.OutputStream
import js7.launcher.forwindows.WindowsApi.{call, kernel32}

private abstract class PipeOutputStream(pipeHandle: HANDLE) extends OutputStream:
  def write(b: Int) = write(Array(b.toByte), 0, 1)

  override def write(bytes: Array[Byte], offset: Int, length: Int) =
    val a = if offset == 0 then bytes else bytes.slice(offset, offset + length)
    require(length <= a.length, "Invalid length")

    val bytesWritten = new IntByReference
    call("WriteFile")(
      kernel32.WriteFile(pipeHandle, bytes, length, bytesWritten, null))
    if bytesWritten.getValue != length then
      sys.error(s"Less bytes written (${bytesWritten.getValue} to process' stdin than expected ($length)")

  //override def flush() =
  //  call("FlushFileBuffers") {
  //    kernel32.FlushFileBuffers(pipeHandle)  // Error "WINDOWS-109 Die Pipe wurde beendet" in JS861IT
  //  }
