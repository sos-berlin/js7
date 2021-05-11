package js7.base.crypt.x509

import java.nio.file.Files.{delete, exists}
import java.nio.file.{Path, Paths}
import js7.base.auth.Pem
import js7.base.crypt.SignerId
import js7.base.crypt.x509.Openssl._
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.Processes.runProcess
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.{isMac, isWindows}
import js7.base.utils.ScalaUtils.syntax.RichBoolean

final class Openssl(dir: Path)
{
  // See https://legacy.thomas-leister.de/eine-eigene-openssl-ca-erstellen-und-zertifikate-ausstellen/

  private lazy val systemOpensslCnf: String = {
    val OpensslDir = """OPENSSLDIR: "(.*)"""".r
    val opensslCnf = runProcess(s"$openssl version -a").split('\n').collectFirst {
      case OpensslDir(dir) =>
        val dir_ = if (isWindows && dir.startsWith("/etc")) "c:/cygwin64" + dir else dir
        (Paths.get(dir_) / "openssl.cnf").contentString
    }.getOrElse(sys.error(s"Missing OPENSSLDIR in output of: $openssl version -a"))
    logger.debug(s"Using system openssl.cnf $opensslCnf")
    opensslCnf
  }
  // In openssl 1.1.1i this has already be done, so this will no longer be required !!!
  private lazy val caConstraintFile = {
    val f = dir / "openssl-ca.cnf"
    f := systemOpensslCnf + """
        |[SAN]
        |basicConstraints = critical, CA:TRUE
        |""".stripMargin
    f
  }
  private lazy val noCaConstraintFile = {
    val f = dir / "openssl-no-ca.cnf"
    f := systemOpensslCnf + """
        |[SAN]
        |basicConstraints = CA:FALSE
        |""".stripMargin
    f
  }

  def selectOpensslConf(ca: Boolean) =
    if (ca) caConstraintFile else noCaConstraintFile

  // suppressCAContraint does not work since openssl 1.1.1i !!!
  final class Root(name: String, suppressCAContraint: Boolean = false) {
    root =>
    val privateKeyFile = dir / s"$name.private-key"
    val certificateFile = dir / s"$name.crt"

    opensslReq(s"/CN=$name", privateKeyFile, certificateFile, ca = !suppressCAContraint)

    private def newCertificate(csrFile: Path, name: String): Path = {
      val certFile = dir / s"$name.crt"
      runProcess(s"$openssl x509 -req -in ${quote(csrFile)} -CA ${quote(root.certificateFile)} -CAkey ${quote(privateKeyFile)}" +
        s" -CAcreateserial -days 2 -sha512 -out ${quote(certFile)}")
      assertPemFile("CERTIFICATE", certFile)
      certFile
    }

    final class Signer(name: String)
    {
      val signerId = SignerId(s"CN=$name")
      private val privateKeyFile = dir / s"$name.private-key"

      val certificateFile = {
        runProcess(s"$openssl genrsa -out ${quote(privateKeyFile)} 1024")
        val certificateRequestFile = dir / s"$name.csr"
        runProcess(s"$openssl req -new -subj '/${signerId.string}' -key ${quote(privateKeyFile)} -out ${quote(certificateRequestFile)}")
        val certFile = root.newCertificate(certificateRequestFile, name)
        delete(certificateRequestFile)
        certFile
      }

      /** Return a signature file (binary). */
      def sign(documentFile: Path): Path = {
        val signatureFile = Paths.get(s"$documentFile.signature")
        runProcess(s"$openssl dgst -sha512 -sign ${quote(privateKeyFile)} -out ${quote(signatureFile)} ${quote(documentFile)}")

        //if (false) { /*verify generated signature*/
        //  val publicKeyFile = Paths.get(s"$documentFile.public-key")
        //  runProcess(s"""sh -c '$openssl x509 -pubkey -noout -in "$certificateFile" >"$publicKeyFile"'""")
        //  runProcess(s"$openssl dgst -sha512 -verify '$publicKeyFile' -signature '$signatureFile' '$documentFile'")
        //}

        // Convert to MIME base64 (required for JS7 updateRepo)
        val base64SignatureFile = Paths.get(s"$signatureFile.base64")
        runProcess(s"$openssl base64 -in ${quote(signatureFile)} -out ${quote(base64SignatureFile)}")
        delete(signatureFile)
        base64SignatureFile
      }
    }
  }

  def generateCertWithPrivateKey(name: String, distinguishedName: String)
  : Checked[CertWithPrivateKey] = {
    val privateFile = dir / s"$name.private-key.pem"
    val certFile = dir / s"$name.certificate.pem"

    opensslReq(distinguishedName, privateFile, certFile, ca = false)

    for (privateKey <- PrivateKeyPem.fromPem(privateFile.contentString)) yield
      CertWithPrivateKey(privateKey = privateKey, certificate = certFile.byteArray)
  }

  private def opensslReq(distinguishedName: String, privateFile: Path, certFile: Path, ca: Boolean) =
    runProcess(s"$openssl req -x509 -newkey rsa:1024 -sha512 -days 2 -nodes " +
      s"-subj '$distinguishedName' " +
      s"-keyout ${quote(privateFile)} " +
      s"-out ${quote(certFile)} " +
      (ca ?? s"-extensions 'SAN' -config ${quote(caConstraintFile)}"))
}

object Openssl
{
  private val logger = Logger[this.type]
  private val PrivateKeyPem = Pem("PRIVATE KEY")

  private val useHomebrew = true
  private lazy val homebrewOpenssl = Paths.get("/usr/local/opt/openssl/bin/openssl")
  lazy val openssl = {
    val openssl =
      if (useHomebrew && isMac && exists(homebrewOpenssl)) homebrewOpenssl
      else Paths.get("openssl")
    val version = runProcess(s"$openssl version").trim
    logger.info(s"Using $version, $openssl")
    quote(openssl)
  }

  def assertPemFile(typ: String, file: Path): Unit =
    Pem(typ).fromPem(file.contentString).orThrow

  final case class CertWithPrivateKey(privateKey: ByteArray, certificate: ByteArray)

  // For Windows
  def quote(path: Path) = "'" + path.toString.replace("\\", "\\\\") + "'"
}
