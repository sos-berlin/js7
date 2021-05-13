package js7.executor.forwindows

import com.sun.jna.Structure
import com.sun.jna.ptr.PointerByReference
import java.nio.charset.StandardCharsets.UTF_16LE
import js7.base.generic.SecretString
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.executor.forwindows.WindowsApi.{MyAdvapi32, call, myAdvapi32}
import scala.util.control.NonFatal

private[executor] final case class WindowsProcessCredential(
  userName: WindowsUserName,
  password: SecretString)

object WindowsProcessCredential
{
  private[executor] def byKey(key: String): Checked[WindowsProcessCredential] =
    readCredential(key) { cred =>
      val passwordBytes = cred.credentialBlob.getByteArray(0, cred.credentialBlobSize)
      val password = SecretString(new String(passwordBytes, UTF_16LE))
      java.util.Arrays.fill(passwordBytes, 0: Byte)
      WindowsProcessCredential(toUser(cred), password)
    }

  def keyToUser(key: String): Checked[WindowsUserName] =
    readCredential(key)(toUser)

  private def toUser(cred: CREDENTIAL): WindowsUserName =
    WindowsUserName(cred.userName.toString)

  private def readCredential[A](key: String)(read: CREDENTIAL => A): Checked[A] =
    if (!isWindows)
      Left(Problem.pure("Windows credential can only be read under Microsoft Windows"))
    else
      try {
        val ref = new PointerByReference
        call("CredRead")(
          myAdvapi32.CredRead(key, MyAdvapi32.CRED_TYPE_GENERIC, 0, ref))
        try {
          val credential = Structure.newInstance(classOf[CREDENTIAL], ref.getValue)
          credential.read()
          val result = read(credential)
          credential.clear()
          Right(result)
        } finally myAdvapi32.CredFree(ref.getValue)
      } catch { case NonFatal(e: Exception) =>
        Left(Problem.pure(
          s"Windows Credential Manager does not return an entry named '$key': ${e.getMessage}"))
      }
}
