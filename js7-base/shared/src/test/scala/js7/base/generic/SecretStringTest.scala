package js7.base.generic

import java.lang.System.nanoTime
import js7.base.test.OurTestSuite
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class SecretStringTest extends OurTestSuite
{
  "provideCharArray deletes provided characters" in {
    var a: Array[Char] = null
    SecretString("SECRET").provideCharArray { chars =>
      assert(chars sameElements Array('S', 'E', 'C', 'R', 'E', 'T'))
      a = chars
    }
    assert(a sameElements Array.fill(6)('\u0000'))
  }

  "provideCharArray deletes provided characters on exception" in {
    var a: Array[Char] = null
    intercept[IllegalStateException] {
      SecretString("SECRET").provideCharArray {
        chars => a = chars
        throw new IllegalStateException
      }
    }
    assert(a sameElements Array.fill(6)('\u0000'))
  }

  "isEmpty" in {
    assert(SecretString("").isEmpty)
    assert(!SecretString("X").isEmpty)
  }

  "nonEmpty" in {
    assert(SecretString("X").nonEmpty)
    assert(!SecretString("").nonEmpty)
  }

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
    assert(secret.toString == "Secret")
    assert(s"$secret" == "Secret")
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
