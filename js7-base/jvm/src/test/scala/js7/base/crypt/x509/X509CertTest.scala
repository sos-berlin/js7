package js7.base.crypt.x509

import js7.base.auth.DistinguishedName
import js7.base.crypt.SignerId
import js7.base.crypt.x509.Openssl.openssl
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.process.Processes.runProcess
import js7.base.problem.Checked.*
import org.scalatest.freespec.AnyFreeSpec

final class X509CertTest extends AnyFreeSpec
{
  "signerId" in {
    withTemporaryDirectory("X509CertTest-") { dir =>
      val privateKeyFile = dir / "signer.key"
      val certificateFile = dir / "signer.crt"
      runProcess(s"$openssl req -x509 -newkey rsa:1024 -sha512 -days 2  -nodes -subj '/L=Berlin/CN=TESTER' " +
        s"-keyout '$privateKeyFile' -out '$certificateFile'")
      val cert = X509Cert.fromPem(certificateFile.contentString).orThrow
      assert(cert.signerId == SignerId("CN=TESTER, L=Berlin"))
      assert(cert.signersDistinguishedName == DistinguishedName("CN=TESTER, L=Berlin"))
      assert(cert.signersDistinguishedName == DistinguishedName("CN = TESTER ,  L = Berlin "))
      assert(cert.signersDistinguishedName != DistinguishedName("CN=Alien, L=Berlin"))
    }
  }
}
