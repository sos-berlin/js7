package js7.base.auth

import js7.base.generic.SecretString
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class HashedPasswordTest extends OurTestSuite:

  "equalsClearText" in:
    val a = HashedPassword(SecretString("321"), _.reverse)
    assert(a.hasher("123") == a.hashed.string)
    assert(a.equalsClearText(SecretString("123")))
    assert(!a.equalsClearText(SecretString("321")))

  "hashAgainRandom" in:
    val a = HashedPassword(SecretString("321"), _.reverse)
    val b = a.hashAgainRandom
    val c = a.hashAgainRandom
    assert(b.hashed.string != c.hashed.string)
    assert(b.hashAgainRandom.hashed.string != c.hashAgainRandom.hashed.string)
    assert(a equalsClearText SecretString("123"))
    assert(b equalsClearText SecretString("123"))
    assert(c equalsClearText SecretString("123"))

  "empty" in:
    val a = HashedPassword.newEmpty()
    val b = HashedPassword.newEmpty()
    assert(a != b)
    assert(a.hashed != b.hashed)
    assert(a.hashed.string != b.hashed.string)
    assert(a equalsClearText SecretString(""))
    assert(b equalsClearText SecretString(""))
    assert(a.hasher("") == a.hashed.string)
    assert(b.hasher("") == b.hashed.string)

  "MatchesNothing" in:
    assert(!HashedPassword.MatchesNothing.equalsClearText(SecretString("")))
    assert(!HashedPassword.MatchesNothing.equalsClearText(SecretString(HashedPassword.MatchesNothingString)))
    assert(HashedPassword.MatchesNothing.toString == "HashedPassword(MatchesNothing)")
