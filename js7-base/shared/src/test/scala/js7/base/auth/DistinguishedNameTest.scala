package js7.base.auth

import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class DistinguishedNameTest extends AnyFreeSpec
{
  "check" in {
    assert(DistinguishedName.checked("") == Left(EmptyStringProblem("DistinguishedName")))
    assert(DistinguishedName.checked(" ") == Left(EmptyStringProblem("DistinguishedName")))
    assert(DistinguishedName.checked("X") == Left(Problem("Invalid Distinguished Name - improperly specified input name: X")))
    assert(DistinguishedName.checked("UNKNOWN=X") == Left(Problem("Invalid Distinguished Name - improperly specified input name: UNKNOWN=X")))
    assert(DistinguishedName.checked("CN=common name") == Right(DistinguishedName("CN=common name")))
  }

  "DistinguishedName, equal" in {
    val a = DistinguishedName("CN=common name,L=Lummerland")
    val b = DistinguishedName("CN=common name, L=Lummerland")
    assert(a == b)
  }

  "string representation is normalized" in {
    assert(DistinguishedName("CN=common name,L=Lummerland").string == "CN=common name, L=Lummerland")
    assert(DistinguishedName(" CN=common name ,  L=Lummerland ").string == "CN=common name, L=Lummerland")
  }

  "openssh notation is rejected" in {
    assert(DistinguishedName.checked("/L=Lummerland/CN=common name") ==
      Left(Problem("Invalid Distinguished Name - improperly specified input name: /L=Lummerland/CN=common name")))
  }
}
