package js7.common.auth

import js7.base.generic.SecretString
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTime
import js7.common.auth.SecretStringGenerator.newSecretString
import js7.common.auth.SecretStringGeneratorTest._
import js7.common.scalautil.Logger
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class SecretStringGeneratorTest extends AnyFreeSpec {

  "newSecretString returns different secrets" in {
    logger.debug(newSecretString().string)
    val n = 100000
    val secrets = for (_ <- 1 to n) yield newSecretString()
    assert(secrets.distinct.size == n)
    assert(secrets.map(_.string).distinct.size == n)
    assert(secrets.distinct == secrets)
  }

  "newSecretString returns restricted character set" in {
    for (_ <- 1 to 1000) {
      val secretString = newSecretString().string
      assert(secretString forall ExpectedCharacters)
      assert(secretString.size == SecretSize)
    }
  }

  "newSecretString is fast" in {
    val result = measureTime(10000, "newSecretString") { newSecretString() }
    assert(result.singleDuration < 1.ms)
  }

  "SecretString.toString does not show secret" in {
    assert(SecretString("secret").toString == "SecretString")
  }
}

private object SecretStringGeneratorTest {
  private val logger = Logger(getClass)
  private val ExpectedCharacters = Set('-', '_') ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')
  private val SecretSize = 24   // = 28/3*4
}
