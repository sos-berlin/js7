package js7.core.crypt.x509

import java.nio.file.Files.{delete, exists}
import java.nio.file.{Path, Paths}
import js7.base.auth.Pem
import js7.base.problem.Checked._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.process.Processes.runProcess
import js7.common.scalautil.FileUtils.syntax._
import js7.core.crypt.x509.OpensslContext.assertPemFile

final class OpensslContext(dir: Path) {
  // See https://legacy.thomas-leister.de/eine-eigene-openssl-ca-erstellen-und-zertifikate-ausstellen/

  final class Root(name: String, suppressCAContraint: Boolean = false) {
    root =>
    val privateKeyFile = dir / s"$name.private-key"
    val certificateFile = dir / s"$name.crt"

    runProcess(s"openssl genrsa -out '$privateKeyFile' 1024")

    private val systemOpensslCnf: String = {
      val cnf1 = Paths.get("/etc/ssl/openssl.cnf")
      val cnf2 = Paths.get("/etc/pki/tls/openssl.cnf")
      (if (!exists(cnf1) && exists(cnf2)) cnf2 else cnf1).contentString
    }
    private val rootCaConstraintFile = {
      val f = dir / "openssl-ext.cnf"
      f := systemOpensslCnf + """
          |[SAN]
          |basicConstraints = critical, CA:TRUE
          |""".stripMargin
      f
    }
    runProcess(s"openssl req -x509 -new -subj '/CN=$name' -key '$privateKeyFile' " +
      s"-days 2 -nodes -out '$certificateFile' -sha512 " +
      (!suppressCAContraint ?? s"-extensions 'SAN' -config '$rootCaConstraintFile'"))

    private def newCertificate(csrFile: Path, name: String): Path = {
      val certFile = dir / s"$name.crt"
      runProcess(s"openssl x509 -req -in '$csrFile' -CA '${root.certificateFile}' -CAkey '$privateKeyFile'" +
        s" -CAcreateserial -days 2 -sha512 -out '$certFile'")
      assertPemFile("CERTIFICATE", certFile)
      certFile
    }

    final class Signer(name: String)
    {
      private val privateKeyFile = dir / s"$name.private-key"

      val certificateFile = {
        runProcess(s"openssl genrsa -out '$privateKeyFile' 1024")
        val certificateRequestFile = dir / s"$name.csr"
        runProcess(s"openssl req -new -subj '/CN=$name' -key '$privateKeyFile' -out '$certificateRequestFile'")
        val certFile = root.newCertificate(certificateRequestFile, name)
        delete(certificateRequestFile)
        certFile
      }

      /** Return a signature file (binary). */
      def sign(documentFile: Path): Path = {
        val signatureFile = Paths.get(documentFile + ".signature")
        runProcess(s"openssl dgst -sha512 -sign '$privateKeyFile' -out '$signatureFile' '$documentFile'")

        if (false) { /*verifiy generated signature*/
          val publicKeyFile = Paths.get(documentFile + ".public-key")
          runProcess(s"""sh -c 'openssl x509 -pubkey -noout -in "$certificateFile" >"$publicKeyFile"'""")
          runProcess(s"openssl dgst -sha512 -verify '$publicKeyFile' -signature '$signatureFile' '$documentFile'")
        }

        // Convert to MIME base64 (required for JS7 updateRepo)
        val base64SignatureFile = Paths.get(signatureFile + ".base64")
        runProcess(s"openssl base64 -in '$signatureFile' -out '$base64SignatureFile'")
        delete(signatureFile)
        base64SignatureFile
      }
    }
  }
}

object OpensslContext
{
  def assertPemFile(typ: String, file: Path): Unit =
    Pem(typ).fromPem(file.contentString).orThrow
}
