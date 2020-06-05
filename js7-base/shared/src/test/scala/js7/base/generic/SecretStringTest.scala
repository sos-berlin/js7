package js7.base.generic

import java.lang.System.nanoTime
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class SecretStringTest extends AnyFreeSpec {

  "equals" in {
    assert(SecretString("") == SecretString(""))
    assert(SecretString("abc") != SecretString(""))
    assert(SecretString("") != SecretString("abc"))
    assert(SecretString("abc") == SecretString("abc"))
    assert(SecretString("abc.") != SecretString("abc"))
    assert(SecretString("abc") != SecretString("abc."))
    assert(SecretString("abc") != SecretString("ab."))
  }

  "toString does not disclose the secret" in {
    val secret = SecretString("TEST-SECRET")
    assert(secret.toString == "SecretString")
    assert(s"$secret" == "SecretString")
  }

  "Timing ==" in {
    // SecretString.equals takes equal for each string (String.equals takes shorter time for unequal Strings)
    val secret1 = SecretString(Random.nextString(100000))
    val secret2 = SecretString(Random.nextString(100000))
    val n = 100

    def meter(body: => Boolean) = {
      val t = nanoTime
      for (_ <- 1 to n) body
      nanoTime - t
    }
    val times = for (_ <- 1 to 10) yield (meter(secret1.string == secret2.string), meter(secret1 == secret2))
    assert(times.map(_._1).sum < 10 * times.map(_._2).sum)
  }
}
