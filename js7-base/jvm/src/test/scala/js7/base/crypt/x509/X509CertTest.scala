package js7.base.crypt.x509

import js7.base.auth.DistinguishedName
import js7.base.crypt.SignerId
import js7.base.crypt.x509.Openssl.openssl
import js7.base.crypt.x509.X509CertTest.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.process.Processes.runProcess
import js7.base.problem.Checked.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.Timestamp
import scala.util.Random

final class X509CertTest extends OurAsyncTestSuite
{
  "signerId" in {
    withTemporaryDirectory("X509CertTest-") { dir =>
      val privateKeyFile = dir / "signer.key"
      val certificateFile = dir / "signer.crt"
      runProcess(s"$openssl req -x509 -newkey rsa:1024 -sha512 -days 2  -nodes -subj '/L=Berlin/CN=TESTER' " +
        s"-keyout '$privateKeyFile' -out '$certificateFile'")
      val cert = X509Cert.fromPem(certificateFile.contentString, allowExpiredCert = false).orThrow
      assert(cert.signerId == SignerId("CN=TESTER, L=Berlin"))
      assert(cert.signersDistinguishedName == DistinguishedName("CN=TESTER, L=Berlin"))
      assert(cert.signersDistinguishedName == DistinguishedName("CN = TESTER ,  L = Berlin "))
      assert(cert.signersDistinguishedName != DistinguishedName("CN=Alien, L=Berlin"))
    }
  }

  "removeDuplicates" in {
    def dn(name: String) = DistinguishedName(name)
    def ts(date: String) = Timestamp(date + "T00:00:00Z")

    // No duplicates
    val a = TestCert(dn("CN=A"), ts("2024-02-01"), ts("2024-03-01")) // expired
    val b = TestCert(dn("CN=B"), ts("2024-03-01"), ts("2024-04-01")) // valid
    val c = TestCert(dn("CN=C"), ts("2024-04-01"), ts("2024-05-01")) // coming

    // Duplicates, but expired
    val d1 = TestCert(dn("CN=D"), ts("2024-03-01"), ts("2024-03-10")) // expired
    val d2 = TestCert(dn("CN=D"), ts("2024-03-10"), ts("2024-03-22")) // expired

    // expired, valid and coming
    val e1 = TestCert(dn("CN=E"), ts("2024-03-01"), ts("2024-03-10")) // expired
    val e2 = TestCert(dn("CN=E"), ts("2024-03-10"), ts("2024-03-31")) // valid
    val e3 = TestCert(dn("CN=E"), ts("2024-03-11"), ts("2024-03-30")) // valid
    val e4 = TestCert(dn("CN=E"), ts("2024-03-12"), ts("2024-04-01")) // valid BEST
    val e5 = TestCert(dn("CN=E"), ts("2024-03-21"), ts("2024-03-29")) // valid
    val e6 = TestCert(dn("CN=E"), ts("2024-03-23"), ts("2024-03-26")) // coming
    val e7 = TestCert(dn("CN=E"), ts("2024-03-24"), ts("2024-03-25")) // coming
    val e8 = TestCert(dn("CN=E"), ts("2024-03-25"), ts("2024-03-29")) // coming

    // Coming
    val f1 = TestCert(dn("CN=F"), ts("2024-03-25"), ts("2024-03-26")) // coming
    val f2 = TestCert(dn("CN=F"), ts("2024-03-23"), ts("2024-03-27")) // coming BEST
    val f3 = TestCert(dn("CN=F"), ts("2024-03-24"), ts("2024-03-29")) // coming

    val certs = Random.shuffle(Seq(a, b, c, d1, d2, e1, e2, e3, e4, e5, e6, e7, e8, f1, f2, f3))

    assert(X509Cert.removeDuplicates(certs, ts("2024-03-22")).sortBy(_.toString) ==
      Seq(a, b, c, d2, e4, f2))
  }
}

object X509CertTest {
  private final case class TestCert(
    signersDistinguishedName: DistinguishedName,
    notBefore: Timestamp,
    notAfter: Timestamp)
    (implicit name: sourcecode.Name)
  extends X509CertInterface{
    override def toString = name.value
  }
}
