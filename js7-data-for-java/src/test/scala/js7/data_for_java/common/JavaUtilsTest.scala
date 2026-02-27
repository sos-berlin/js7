package js7.data_for_java.common

import js7.base.test.OurTestSuite
import js7.data_for_java.common.JavaUtils.assertTypeForJava

final class JavaUtilsTest extends OurTestSuite:

  "assertTypeForJava" in:
    def f(a: String | Int): Unit =
      assertTypeForJava(a)

    locally:
      val e = intercept[ClassCastException]:
        f(true.asInstanceOf[String])
      assert(e.toString ==
        "java.lang.ClassCastException: class java.lang.Boolean cannot be cast to " +
          "class java.lang.String (java.lang.Boolean and java.lang.String are in " +
          "module java.base of loader 'bootstrap')")

    locally:
      val e = intercept[NullPointerException]:
        f(null.asInstanceOf[String])
      assert(e.toString == "java.lang.NullPointerException")
