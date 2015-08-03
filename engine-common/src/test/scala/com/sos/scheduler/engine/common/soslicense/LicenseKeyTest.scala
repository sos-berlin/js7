package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter
import com.sos.scheduler.engine.common.soslicense.Parameters._
import java.time.LocalDate
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class LicenseKeyTest extends FreeSpec {

  testLicenseKey("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6") { key ⇒
    assert(key.toString == "SOS-DEMO-1-D3Q-1AWS-ZZ")
    assert(key.issuer == "SOS")
    assert(key.customer == "DEMO")
    assert(key.issuedAt == LocalDate.of(2003, 3, 26))
    assert(key.serialNumber == 1)
    assert(key.settings == Map(OperatingSystems → "WS", ZZ → ""))
    assert(key.isUniversalKey)
    assert(key.securityCode == 406925453)
    assert(key.salt == 73)
    assert(key.isValidToday)
    assert(key contains UniversalAgent)
    assert(key contains ZZ)
    assert(!(key contains Parameter("XX")))
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
    assert(!key.isUniversalKey)
    assert(!(key contains UniversalAgent))
    assert(!(key contains ZZ))
  }

  testLicenseKey("SOS-DEMO-1-F2D-2-4-7-A-1AWS-ZHZAF9O") { key ⇒
    assert(key.issuer == "SOS")
    assert(key.customer == "DEMO")
    assert(key.issuedAt == LocalDate.of(2005, 2, 13))
    assert(key.serialNumber == 1)
    assert(key.settings == Map(Parameter("2") → "", Parameter("4") → "", Parameter("7") → "", Parameter("A") → "", OperatingSystems → "WS"))
    assert(key.isValidToday)
    assert(!key.isUniversalKey)
    assert(!(key contains UniversalAgent))
    assert(!(key contains ZZ))
    assert(key contains Parameter("2"))
    assert(key contains Parameter("4"))
    assert(key contains JobScheduler)
    assert(key contains Parameter("A"))
  }

  testLicenseKey("SOS-DEMO-1-L2O-4-7-22-KL22SL7") { key ⇒
    assert(key.issuer == "SOS")
    assert(key.customer == "DEMO")
    assert(key.issuedAt == LocalDate.of(2011, 2, 24))
    assert(key.serialNumber == 1)
    assert(key.settings == Map(Parameter("4") → "", Parameter("7") → "", ClassicAgent → ""))
    assert(key.isValidToday)
    assert(!key.isUniversalKey)
    assert(!(key contains UniversalAgent))
    assert(!(key contains ZZ))
    assert(key contains Parameter("4"))
    assert(key contains JobScheduler)
    assert(key contains ClassicAgent)
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
}
