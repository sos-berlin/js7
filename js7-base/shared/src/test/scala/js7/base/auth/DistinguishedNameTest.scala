package js7.base.auth

import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
final class DistinguishedNameTest extends org.scalatest.FreeSpec
{
  "check" in {
    assert(DistinguishedName.checked("") == Left(EmptyStringProblem("DistinguishedName")))
    assert(DistinguishedName.checked(" ") == Left(Problem("Invalid Distinguished Name - Invalid name:  ")))
    assert(DistinguishedName.checked("X") == Left(Problem("Invalid Distinguished Name - Invalid name: X")))
    assert(DistinguishedName.checked("CN=common name") == Right(DistinguishedName("CN=common name")))
  }

  "DistinguishedName" in {
    val a = DistinguishedName("CN=common name,L=Lummerland")
    val b = DistinguishedName("CN=common name, L=Lummerland")
    assert(a == b)
    assert(a.string == "CN=common name,L=Lummerland")
  }
}
