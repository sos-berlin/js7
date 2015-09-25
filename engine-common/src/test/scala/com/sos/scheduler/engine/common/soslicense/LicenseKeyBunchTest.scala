package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter
import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter.{Expired, Missing, OK}
import com.sos.scheduler.engine.common.soslicense.Parameters._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class LicenseKeyBunchTest extends FreeSpec {

  "Empty" in {
    assert(LicenseKeyBunch() == LicenseKeyBunch(""))
    assert(LicenseKeyBunch().keys.isEmpty)
  }

  "Single key" in {
    val keyBunch = LicenseKeyBunch("SOS-DEMO-1-L2O-4-7-22-KL22SL7")
    assert(keyBunch(Parameter("4")) == OK)
    assert(keyBunch(JobScheduler) == OK)
    assert(keyBunch(ClassicAgent) == OK)
    assert(keyBunch(UniversalAgent) == Missing)
    assert(keyBunch(ZZ) == Missing)
  }

  "Single universal (ZZ) key" in {
    val keyBunch = LicenseKeyBunch("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6")
    assert(keyBunch.toString == "SOS-DEMO-1-D3Q-1AWS-ZZ")
    keyBunch.require(UniversalAgent)
    assert(keyBunch(ZZ) == OK)
    intercept[LicenseKeyParameterIsMissingException] { keyBunch.require(Parameter("XX")) }
  }

  "Multiple keys" in {
    val keyBunch = LicenseKeyBunch("SOS-DEMO-1-F2D-2-4-7-A-1AWS-ZHZAF9O SOS-DEMO-1-L2O-4-7-22-KL22SL7")
    assert(keyBunch(JobScheduler) == OK)
    assert(keyBunch(ClassicAgent) == OK)
    assert(keyBunch(UniversalAgent) == Missing)
    assert(keyBunch(ZZ) == Missing)
    for (p ← List("2", "4", "7", "A", "1A", "22")) assert(keyBunch(Parameter(s"$p")) == OK)
    intercept[LicenseKeyParameterIsMissingException] { keyBunch.require(Parameter("3")) }
  }

  "One key has expired" in {
    val keyBunch = LicenseKeyBunch("SOS-DEMO-1-F2D-2-4-7-A-1AWS-ZHZAF9O SOS-DEMO-1-94S-19990531-1AW-ZZ-J1BVPQW")
    assert(keyBunch(JobScheduler) == OK)
    assert(keyBunch(ClassicAgent) == Expired)
    assert(keyBunch(UniversalAgent) == Expired)
    assert(keyBunch(ZZ) == Expired)
    for (p ← List("2", "4", "7", "A", "1A")) assert(keyBunch(Parameter(s"$p")) == OK)
    intercept[LicenseKeyParameterIsMissingException] { keyBunch.require(Parameter("3")) }
  }

  "LicenseKeyBunch must contain only valid keys" in {
    val validKey = "SOS-DEMO-1-L2O-4-7-22-KL22SL7"
    LicenseKeyBunch(validKey)
    intercept[InvalidLicenseKeyException] { LicenseKeyBunch(s"$validKey SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6X") }
    intercept[InvalidLicenseKeyException] { LicenseKeyBunch(s"$validKey SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q") }
    intercept[InvalidLicenseKeyException] { LicenseKeyBunch(s"$validKey SOS-DEMO-1-D3Q-1AWX-ZZ-ITOT9Q6") }
    intercept[InvalidLicenseKeyException] { LicenseKeyBunch(s"$validKey SOS-DEMO-2-D3Q-1AWS-ZZ-ITOT9Q6") }
  }

  "Spaces are ignored" in {
    assert(LicenseKeyBunch("  SOS-DEMO-1-L2O-4-7-22-KL22SL7  SOS-DEMO-1-94S-19990531-1AW-ZZ-J1BVPQW  ") ==
      LicenseKeyBunch("SOS-DEMO-1-L2O-4-7-22-KL22SL7 SOS-DEMO-1-94S-19990531-1AW-ZZ-J1BVPQW"))
  }

  "toString truncates security code" in {
    assert(LicenseKeyBunch("SOS-DEMO-1-L2O-4-7-22-KL22SL7 SOS-DEMO-1-94S-19990531-1AW-ZZ-J1BVPQW").toString ==
      "SOS-DEMO-1-L2O-4-7-22 SOS-DEMO-1-94S-19990531-1AW-ZZ")
  }

  "Exception messages" in {
    val keyBunch = LicenseKeyBunch("SOS-DEMO-1-F2D-2-4-7-A-1AWS-ZHZAF9O SOS-DEMO-1-94S-19990531-1AW-ZZ-J1BVPQW")
    intercept[LicenseKeyParameterIsMissingException] { keyBunch.require(Parameter("XX")) } .getMessage should include ("License key required")
    intercept[LicenseKeyParameterExpiredException] { keyBunch.require(ClassicAgent) } .getMessage should include ("License key expired")
    intercept[InvalidLicenseKeyException] { LicenseKeyBunch(s"INVALID") } .getMessage should include ("Invalid license key")
  }
}
