package com.sos.scheduler.engine.common.auth

import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.auth.EncodedPasswordValidatorTest._
import com.sos.scheduler.engine.common.configutils.Configs.ConvertibleConfig
import com.typesafe.config.ConfigFactory
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class EncodedPasswordValidatorTest extends FreeSpec {

  "Unknown user" in {
    assert(Validator.hashedPasswordOption("UNKNOWN") == None)
    assert(!Validator(UserAndPassword("UNKNOWN", SecretString("WRONG-PASSWORD"))))
  }

  "Wrong password" in {
    assert(!Validator.validatePassword(PlainUser → SecretString("WRONG-PASSWORD"))(PlainConfiguredPassword))
    assert(!Validator.validatePassword(Sha512User → SecretString("WRONG-PASSWORD"))(Sha512ConfiguredPassword))
    assert(!Validator(PlainUser → SecretString("WRONG-PASSWORD")))
    assert(!Validator(Sha512User → SecretString("WRONG-PASSWORD")))
  }

  "Plain password" in {
    assert(Validator.hashedPasswordOption(PlainUser) contains SecretString("plain:PLAIN-PASSWORD"))
    assert(Validator.validatePassword(PlainUser → SecretString("PLAIN-PASSWORD"))(PlainConfiguredPassword))
    assert(Validator(PlainUser → SecretString("PLAIN-PASSWORD")))
  }

  "SHA512 hashed password" in {
    assert(Validator.validatePassword(Sha512User → SecretString("SHA512-PASSWORD"))(Sha512ConfiguredPassword))
    assert(Validator(Sha512User → SecretString("SHA512-PASSWORD")))
  }
}

private object EncodedPasswordValidatorTest {
  private val PlainUser = "PLAIN-USER"
  private val Sha512User = "SHA512-USER"
  private val PlainConfiguredPassword = SecretString("plain:PLAIN-PASSWORD")
  private val Sha512ConfiguredPassword = SecretString(  // "SHA512-PASSWORD"
    "sha512:130c7809c9e5a8d81347b55f5c82c3a7407f4b41b461eb641887d276b11af4b575c5a32d1cf104e531c700e4b1ddd75b27b9e849576f6dfb8ca42789fbc7ece2")

  private val TestConfigValidator = ConfigFactory.parseMap(Map(
    PlainUser → PlainConfiguredPassword.string,
    Sha512User → Sha512ConfiguredPassword.string))

  private val Validator = new EncodedPasswordValidator(TestConfigValidator.optionAs[SecretString])
}
