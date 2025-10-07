package js7.base.crypt.x509

import com.typesafe.config.ConfigFactory
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.SignerId
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{Timestamp, WallClock}
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.DurationInt

final class X509SignatureVerifierTest extends OurTestSuite:

  "now is between certificats's notBefore and notAfter" in:
    val ts = ts"2050-10-03T12:00:00Z"
    assert:
      signAndVerifiy(ts, notBefore = ts - 1.day, notAfter = ts + 1.day, allowExpired = false).isRight
    assert:
      signAndVerifiy(ts, notBefore = ts - 1.day, notAfter = ts + 1.day, allowExpired = true).isRight
    assert:
      signAndVerifiy(ts, notBefore = ts, notAfter = ts, allowExpired = false).isRight
    assert:
      signAndVerifiy(ts, notBefore = ts, notAfter = ts, allowExpired = true).isRight

  "now is later than notAfter" in:
    val ts = ts"2050-10-03T12:00:00Z"
    assert:
      signAndVerifiy(ts, notBefore = ts - 2.day, notAfter = ts - 1.day, allowExpired = false) ==
        Left(Problem:
          "java.security.cert.CertificateExpiredException: NotAfter: Sun Oct 02 14:00:00 CEST 2050")
    assert:
      signAndVerifiy(ts, notBefore = ts - 2.day, notAfter = ts - 1.day, allowExpired = true).isRight

    assert:
      signAndVerifiy(ts, notBefore = ts - 2.s, notAfter = ts - 1.s, allowExpired = false) ==
        Left(Problem:
          "java.security.cert.CertificateExpiredException: NotAfter: Mon Oct 03 13:59:59 CEST 2050")
    assert:
      signAndVerifiy(ts, notBefore = ts - 2.s, notAfter = ts - 1.s, allowExpired = true).isRight

  "now is earlier than notEarlier" in:
    val ts = ts"2050-10-03T12:00:00Z"
    assert:
      signAndVerifiy(ts, notBefore = ts + 1.day, notAfter = ts + 2.days, allowExpired = false) ==
        Left(Problem:
          "java.security.cert.CertificateNotYetValidException: NotBefore: Tue Oct 04 14:00:00 CEST 2050")
    assert:
      signAndVerifiy(ts, notBefore = ts + 1.day, notAfter = ts + 2.days, allowExpired = true).isRight

    assert:
      signAndVerifiy(ts, notBefore = ts + 1.s, notAfter = ts + 1.s, allowExpired = false) ==
        Left(Problem:
          "java.security.cert.CertificateNotYetValidException: NotBefore: Mon Oct 03 14:00:01 CEST 2050")
    assert:
      signAndVerifiy(ts, notBefore = ts + 1.s, notAfter = ts + 1.s, allowExpired = true).isRight


  private def signAndVerifiy(
    now: Timestamp,
    notBefore: Timestamp,
    notAfter: Timestamp,
    allowExpired: Boolean)
  : Checked[Unit] =
    val signerId = SignerId("CN=SIGNER")
    X509Signer
      .newSignerAndVerifier(
        signerId, "X509SignatureVerifierTest",
        notBefore = Some(notBefore),
        notAfter = Some(notAfter),
        WallClock.fixed(now),
        if allowExpired then
          config"js7.configuration.allow-expired-certificates = yes"
        else
          ConfigFactory.empty)
      .map: (signer, verifier) =>
        val document = ByteArray("TEST DOCUMENT")
        val signature = signer.sign(document)
        val signerIds = verifier.verify(document, signature).orThrow
        assert(signerIds == Seq(signerId))
