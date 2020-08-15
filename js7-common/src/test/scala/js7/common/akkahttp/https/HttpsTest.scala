package js7.common.akkahttp.https

import java.io.InputStream
import java.lang.{ProcessBuilder => JavaProcessBuilder}
import java.nio.file.Path
import java.security.KeyStore
import java.security.cert.X509Certificate
import js7.base.data.ByteSequence.ops._
import js7.base.generic.SecretString
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.InputStreams.inputStreamToByteVector
import js7.base.utils.ScodecUtils.syntax._
import js7.common.scalautil.FileUtils.{withTemporaryDirectory, withTemporaryFile}
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.sys.process._

/**
  * @author Joacim Zschimmer
  */
final class HttpsTest extends AnyFreeSpec
{
  private val nullLogger = ProcessLogger(_ => ())

  "Read PEM encoded certificate" in {
    withTemporaryFile { keyFile =>
      val keyStore = loadStdout(makeCertCommand("/CN=localhost", keyFile))
      val alias = "TEST"
      assert(keyStore.getCertificate(alias).asInstanceOf[X509Certificate].getIssuerX500Principal.toString == "CN=localhost")
      assert(!keyStore.isKeyEntry(alias))
    }
  }

  "Read multiple PEM encoded certificate" in {
    withTemporaryFile { keyFile =>
      val content =
        executeCommand(makeCertCommand("/CN=A", keyFile))(inputStreamToByteVector) ++
        executeCommand(makeCertCommand("/CN=B", keyFile))(inputStreamToByteVector)
      val keyStore = Https.loadKeyStoreFromInputStream(content.toInputStream, SecretString(""), "TEST", "TEST-KIND")
      assert(keyStore.getCertificate("TEST#1").asInstanceOf[X509Certificate].getIssuerX500Principal.toString == "CN=A")
      assert(keyStore.getCertificate("TEST#2").asInstanceOf[X509Certificate].getIssuerX500Principal.toString == "CN=B")
      assert(!keyStore.isKeyEntry("TEST#1"))
    }
  }

  private def makeCertCommand(principal: String, keyFile: Path): Seq[String] =
    Seq("openssl", "req", "-batch", "-x509", "-newkey", "rsa:1024", "-days", "2", "-nodes",
              "-subj", principal, "-keyout", keyFile.toString)


  "Read PKCS #12" in {
    withTemporaryDirectory("HttpsTest-") { dir =>
      val cmd = s"openssl req -batch -x509 -newkey rsa:1024 -days 2 -nodes -subj '/CN=localhost' -out '$dir/TEST.crt' -keyout '$dir/TEST.key'"
      val rc = cmd.!(nullLogger)
      assert(rc == 0)

      val keyStore = loadStdout(Seq("openssl", "pkcs12", "-export", "-in", s"$dir/TEST.crt", "-inkey", s"$dir/TEST.key",
        "-passout", "pass:TEST-PASSWORD"))
      val alias = "1"
      assert(keyStore.getCertificate(alias).asInstanceOf[X509Certificate].getIssuerX500Principal.toString == "CN=localhost")
      assert(keyStore.isKeyEntry(alias))
    }
  }

  private def loadStdout(command: Seq[String]): KeyStore =
    executeCommand(command) { in =>
      Https.loadKeyStoreFromInputStream(in, SecretString("TEST-PASSWORD"), "TEST", "TEST-KIND")
    }

  private def executeCommand[A](command: Seq[String])(body: InputStream => A): A = {
    val p = new JavaProcessBuilder(command: _*)
    val process = p.start()
    Future { autoClosing(process.getErrorStream()) { stderr => while (stderr.read() != -1) {} } }
    try
      autoClosing(process.getInputStream()) { in =>
        body(in)
      }
    finally process.waitFor()
  }
}
