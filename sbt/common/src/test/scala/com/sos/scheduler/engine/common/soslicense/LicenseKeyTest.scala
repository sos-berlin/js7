package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter
import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter.{Expired, Missing, OK}
import com.sos.scheduler.engine.common.soslicense.Parameters._
import java.time.LocalDate
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class LicenseKeyTest extends FreeSpec {

  testLicenseKey("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6") { key ⇒
    assert(key.toString == "SOS-DEMO-1-D3Q-1AWS-ZZ")
    assert(key.issuer == "SOS")
    assert(key.customer == "DEMO")
    assert(key.issuedAt == LocalDate.of(2003, 3, 26))
    assert(key.serialNumber == 1)
    assert(key.settings == Map(OperatingSystems → "WS", ZZ → ""))
    assert(key.securityCode == 406925453)
    assert(key.salt == 73)
    assert(key.isValidToday)
    assert(key(JobScheduler) == OK)
    assert(key(ClassicAgent) == OK)
    assert(key(UniversalAgent) == OK)
    assert(key(ZZ) == OK)
    assert(key(Parameter("XX")) == Missing)
    key.require(ClassicAgent)
    key.require(UniversalAgent)
    intercept[LicenseKeyParameterIsMissingException] { key.require(Parameter("XX")) }
  }

  testLicenseKey("SOS-DEMO-1-94S-19990531-1AW-ZZ-J1BVPQW") { key ⇒
    assert(key.issuer == "SOS")
    assert(key.customer == "DEMO")
    assert(key.issuedAt == LocalDate.of(1999, 4, 28))
    assert(key.serialNumber == 1)
    assert(key.settings == Map(ValidIn1900 → "990531", OperatingSystems → "W", ZZ → ""))
    assert(!key.isValidToday)
    assert(key(JobScheduler) == Expired)
    assert(key(UniversalAgent) == Expired)
    assert(key(ZZ) == Expired)
    intercept[LicenseKeyParameterExpiredException] { key.require(JobScheduler) }
    intercept[LicenseKeyParameterExpiredException] { key.require(ClassicAgent) }
    intercept[LicenseKeyParameterIsMissingException] { key.require(Parameter("XX")) }
  }

  testLicenseKey("SOS-DEMO-1-F2D-2-4-7-A-1AWS-ZHZAF9O") { key ⇒
    assert(key.issuer == "SOS")
    assert(key.customer == "DEMO")
    assert(key.issuedAt == LocalDate.of(2005, 2, 13))
    assert(key.serialNumber == 1)
    assert(key.settings == Map(Parameter("2") → "", Parameter("4") → "", Parameter("7") → "", Parameter("A") → "", OperatingSystems → "WS"))
    assert(key.isValidToday)
    assert(key(UniversalAgent) == Missing)
    assert(key(ZZ) == Missing)
    assert(key(Parameter("2")) == OK)
    assert(key(Parameter("4")) == OK)
    assert(key(JobScheduler) == OK)
    assert(key(Parameter("A")) == OK)
    assert(key(UniversalAgent) == Missing)
    key.require(JobScheduler)
    intercept[LicenseKeyParameterIsMissingException] { key.require(UniversalAgent) }
  }

  testLicenseKey("SOS-DEMO-1-L2O-4-7-22-KL22SL7") { key ⇒
    assert(key.issuer == "SOS")
    assert(key.customer == "DEMO")
    assert(key.issuedAt == LocalDate.of(2011, 2, 24))
    assert(key.serialNumber == 1)
    assert(key.settings == Map(Parameter("4") → "", Parameter("7") → "", ClassicAgent → ""))
    assert(key.isValidToday)
    assert(key(Parameter("4")) == OK)
    assert(key(JobScheduler) == OK)
    assert(key(ClassicAgent) == OK)
    assert(key(UniversalAgent) == Missing)
    assert(key(ZZ) == Missing)
    key.require(JobScheduler)
    key.require(ClassicAgent)
    intercept[LicenseKeyParameterIsMissingException] { key.require(UniversalAgent) }
  }

  private def testLicenseKey(key: String)(body: LicenseKey ⇒ Unit): Unit = {
    key in body(LicenseKey(key))
  }

  "Lower case letters are treated as upper case" in {
    assert(LicenseKey("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6") == LicenseKey("sos-demo-1-d3q-1aws-zz-itot9q6"))
  }

  "Invalid licences" in {
    intercept[InvalidLicenseKeyException] { LicenseKey("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6X") }
    intercept[InvalidLicenseKeyException] { LicenseKey("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q") }
    intercept[InvalidLicenseKeyException] { LicenseKey("SOS-DEMO-1-D3Q-1AWX-ZZ-ITOT9Q6") }
    intercept[InvalidLicenseKeyException] { LicenseKey("SOS-DEMO-2-D3Q-1AWS-ZZ-ITOT9Q6") }
  }

  "toString truncates security code" in {
    assert(LicenseKey("SOS-DEMO-1-94S-19990531-1AW-ZZ-J1BVPQW").toString == "SOS-DEMO-1-94S-19990531-1AW-ZZ")
  }
}
