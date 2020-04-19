package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.HasTypeInfoTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class HasTypeInfoTest extends AnyFreeSpec
{
  "Default HasTypeInfo" in {
    assert(implicitly[HasTypeInfo[A[Int]]] == HasTypeInfo("HasTypeInfoTest.A[Int]"))
  }

  "Special implicit HasTypeInfo" in {
    case class A(string: String)
    object A {
      implicit val hasTypeInfo = HasTypeInfo[A]("My-A")
    }
    assert(implicitly[HasTypeInfo[A]] == HasTypeInfo("My-A"))
  }
}

private object HasTypeInfoTest {
  private case class A[T](value: T)
}
