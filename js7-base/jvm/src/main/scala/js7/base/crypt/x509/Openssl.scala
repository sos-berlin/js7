package js7.base.crypt.x509

import java.nio.file.Files.{delete, exists}
import java.nio.file.{Path, Paths}
import java.time.ZoneOffset.UTC
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import js7.base.auth.Pem
import js7.base.crypt.x509.Openssl.*
import js7.base.crypt.x509.X509Cert.PrivateKeyPem
import js7.base.crypt.{DocumentSigner, GenericSignature, Signature, SignerId}
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.io.process.Processes.runProcess
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.{isMac, isWindows}
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichBoolean

final class Openssl(dir: Path):
  // See https://legacy.thomas-leister.de/eine-eigene-openssl-ca-erstellen-und-zertifikate-ausstellen/

  private lazy val systemOpensslCnf: String =
    val OpensslDir = """OPENSSLDIR: "(.*)"""".r

    val opensslCnf = runProcess(s"$openssl version -a")
      .split('\n')
      .collectFirst { case OpensslDir(dir) => dir }
      .map: dir =>
        lazy val cygwinOpensslCnf = Paths.get("""c:\cygwin64""") / dir / "openssl.cnf"
        lazy val winGitOpensslCnf = Paths.get("""C:\Program files\Git\usr\ssl""") / "openssl.cnf"
        if isWindows && dir.startsWith("/etc") && exists(cygwinOpensslCnf) then
          cygwinOpensslCnf.contentString
        else if isWindows && dir == "/usr/ssl" && exists(winGitOpensslCnf) then
          winGitOpensslCnf.contentString
        else
          (Paths.get(dir) / "openssl.cnf").contentString
      .getOrElse:
        sys.error(s"Missing OPENSSLDIR in output of: $openssl version -a")

    logger.debug(s"Using system openssl.cnf $opensslCnf")
    opensslCnf

  // In openssl 1.1.1i this has already be done, so this will no longer be required !!!
  private lazy val caConstraintFile =
    val f = dir / "openssl-ca.cnf"
    f := systemOpensslCnf + """
        |[SAN]
        |basicConstraints = critical, CA:TRUE
        |""".stripMargin
    f

  //private lazy val noCaConstraintFile =
  //  val f = dir / "openssl-no-ca.cnf"
  //  f := systemOpensslCnf + """
  //      |[SAN]
  //      |basicConstraints = CA:FALSE
  //      |""".stripMargin
  //  f
  //
  //def selectOpensslConf(ca: Boolean): Path =
  //  if ca then caConstraintFile else noCaConstraintFile

  // suppressCAContraint does not work since openssl 1.1.1i !!!
  final class Root(name: String, suppressCAContraint: Boolean = false):
    root =>
    val privateKeyFile: Path = dir / s"$name.private-key"
    val certificateFile: Path = dir / s"$name.crt"

    opensslReq(s"/CN=$name", privateKeyFile, certificateFile, ca = !suppressCAContraint)

    private def newCertificate(csrFile: Path, name: String): Path =
      val certFile = dir / s"$name.crt"
      runProcess:
        s"$openssl x509 -req" +
          s" -in ${quote(csrFile)}" +
          s" -CA ${quote(root.certificateFile)}" +
          s" -CAkey ${quote(privateKeyFile)}" +
          s" -CAcreateserial" +
          s" -days 2 -sha512 -out ${quote(certFile)}"
      assertPemFile("CERTIFICATE", certFile)
      certFile

    final class Signer(name: String) extends DocumentSigner:
      type MySignature = OpensslSignature
      val companion: Signer.type = Signer
      val signerId: SignerId = SignerId(s"CN=$name")
      private val privateKeyFile = dir / s"$name.private-key"

      val certificateFile: Path =
        runProcess:
          s"$openssl genrsa -out ${quote(privateKeyFile)} 1024"
        val certificateRequestFile = dir / s"$name.csr"
        runProcess:
          s"$openssl req -new" +
            s" -subj '/${signerId.string}'" +
            s" -key ${quote(privateKeyFile)}" +
            s" -out ${quote(certificateRequestFile)}"
        val certFile = root.newCertificate(certificateRequestFile, name)
        delete(certificateRequestFile)
        certFile

      def certificateString: String =
        certificateFile.contentString

      def sign(document: ByteArray): OpensslSignature =
        withTemporaryFile("openssl-", ".tmp"): file =>
          file := document
          val signatureFile = signFile(file)
          val signature = signatureFile.contentString
          delete(signatureFile)
          OpensslSignature(signature)

      /** Return a signature file (binary). */
      def signFile(documentFile: Path): Path =
        val signatureFile = Paths.get(s"$documentFile.signature")
        runProcess:
          s"$openssl dgst -sha512 -sign ${quote(privateKeyFile)}" +
            s" -out ${quote(signatureFile)} ${quote(documentFile)}"

        //if (false) { /*verify generated signature*/
        //  val publicKeyFile = Paths.get(s"$documentFile.public-key")
        //  runProcess(s"""sh -c '$openssl x509 -pubkey -noout -in "$certificateFile" >"$publicKeyFile"'""")
        //  runProcess(s"$openssl dgst -sha512 -verify '$publicKeyFile' -signature '$signatureFile' '$documentFile'")
        //}

        // Convert to MIME base64 (required for JS7 updateRepo)
        val base64SignatureFile = Paths.get(s"$signatureFile.base64")
        runProcess:
          s"$openssl base64 -in ${quote(signatureFile)} -out ${quote(base64SignatureFile)}"
        delete(signatureFile)
        base64SignatureFile

    object Signer extends DocumentSigner.Companion:
      protected type MySignature = OpensslSignature
      protected type MyMessageSigner = Signer

      val typeName = "X509-openssl"

      def checked(privateKey: ByteArray, password: SecretString): Nothing =
        throw new NotImplementedError

  def generateCertWithPrivateKey(
    name: String,
    distinguishedName: String,
    notBefore: Option[Timestamp] = None,
    notAfter: Option[Timestamp] = None)
  : Checked[CertWithPrivateKey] =
    val privateFile = dir / s"$name.private-key.pem"
    val certFile = dir / s"$name.certificate.pem"

    opensslReq(distinguishedName, privateFile, certFile, ca = false,
      notBefore = notBefore, notAfter = notAfter)

    val p12File = dir / s"$name.certificate.p12"
    runProcess:
      s"openssl pkcs12 -export" +
        s" -inkey $privateFile" +
        s" -in $certFile" +
        s" -out $p12File" +
        s" -passout 'pass:$p12Password'"

    for privateKey <- PrivateKeyPem.fromPem(privateFile.contentString) yield
      CertWithPrivateKey(privateKey = privateKey, certificate = certFile.byteArray, p12File)

  private def opensslReq(distinguishedName: String, privateFile: Path, certFile: Path, ca: Boolean,
    notBefore: Option[Timestamp] = None,
    notAfter: Option[Timestamp] = None) =
    runProcess:
      s"$openssl req -x509 -newkey rsa:1024 -sha512 -nodes " +
        s"-subj '$distinguishedName' " +
        s"-keyout ${quote(privateFile)} " +
        s"-out ${quote(certFile)} " +
        (ca ?? s"-extensions 'SAN' -config ${quote(caConstraintFile)}")
        // TODO Doesn't work with Almalinux 10:
        + notBefore.fold(""): ts =>
          s" -not_before ${toTimestampString(ts)}"
        + notAfter.fold(""): ts =>
          s" -not_after ${toTimestampString(ts)}"


object Openssl:
  private val logger = Logger[this.type]
  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss'Z'")
  private val p12Password = "jobscheduler"
  private val useHomebrew = isMac
  private lazy val homebrewOpenssl = Paths.get("/usr/local/opt/openssl/bin/openssl")

  lazy val openssl: String =
    val openssl =
      if useHomebrew && exists(homebrewOpenssl) then
        homebrewOpenssl
      else
        Paths.get("openssl")
    val version = runProcess(s"$openssl version").trim
    logger.info(s"Using $version, $openssl")
    quote(openssl)

  def assertPemFile(typ: String, file: Path): Unit =
    Pem(typ).fromPem(file.contentString).orThrow

  final case class CertWithPrivateKey(privateKey: ByteArray, certificate: ByteArray, p12File: Path):
    lazy val certificatePem: String =
      X509Cert.CertificatePem.toPem(certificate)

  private def toTimestampString(ts: Timestamp): String =
    dateTimeFormatter.format(ZonedDateTime.ofInstant(ts.toInstant, UTC))

  // For Windows
  def quote(path: Path): String =
    "'" + path.toString.replace("\\", "\\\\") + "'"


final case class OpensslSignature(base64: String) extends Signature:
  def toGenericSignature: GenericSignature =
    GenericSignature(X509Signer.typeName, base64)
