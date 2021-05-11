package js7.executor.forwindows

import com.sun.jna.platform.win32.WinError.ERROR_BROKEN_PIPE
import com.sun.jna.platform.win32.WinNT.HANDLE
import com.sun.jna.ptr.IntByReference
import java.io.InputStream
import js7.executor.forwindows.WindowsApi.{call, kernel32}

private abstract class PipeInputStream(pipeHandle: HANDLE) extends InputStream
{
  private lazy val myBuffer = new Array[Byte](4096)

  def read() = {
    val array = Array[Byte](1)
    val len = read(array)
    if (len == 1)
      array(0).toInt & 0xff
    else
      -1
  }

  override def read(buffer: Array[Byte], offset: Int, length: Int) = {
    val (a, len) = if (offset == 0) (buffer, length) else (myBuffer, myBuffer.size min length)
    val bytesRead = new IntByReference
    var brokenPipe = false
    call("ReadFile", "PipeInputStream") {
      val ok = kernel32.ReadFile(pipeHandle, a, len, bytesRead, null)
      if (!ok && kernel32.GetLastError == ERROR_BROKEN_PIPE) {
        brokenPipe = true
        true // no error
      } else ok
    }
    if (brokenPipe)
      -1
    else {
      if (a ne buffer) {
        a.copyToArray(buffer, offset, bytesRead.getValue)
      }
      bytesRead.getValue
    }
  }
}
