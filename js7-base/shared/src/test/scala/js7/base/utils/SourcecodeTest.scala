package js7.base.utils

import java.nio.file.Paths
import js7.base.test.OurTestSuite

final class SourcecodeTest extends OurTestSuite
{
  private val enclosing = implicitly[sourcecode.Enclosing].value
  private val name = implicitly[sourcecode.Name].value

  "sourcecode.Enclosing" in {
    assert(enclosing == "js7.base.utils.SourcecodeTest#enclosing")
  }

  "sourcecode.Name" in {
    assert(name == "name")
  }

  "sourcecode.Pkg" in {
    assert(implicitly[sourcecode.Pkg].value == "js7.base.utils")
  }

  "sourcecode.File" in {
    val file = Paths.get(implicitly[sourcecode.File].value)
    assert(file.isAbsolute && file.endsWith(Paths.get("js7/base/utils/SourcecodeTest.scala")))
  }

  "sourcecode.FileName" in {
    assert(implicitly[sourcecode.FileName].value == "SourcecodeTest.scala")
  }

  "sourcecode.Line" in {
    assert(implicitly[sourcecode.Line].value == 33)
  }

  "sourcecode.Text" in {
    def text(arg: sourcecode.Text[Int]) = arg

    assert(text(7).value == 7)
    assert(text(7).source == "7")

    assert(text(3 * 7).value == 21)
    assert(text(3 *7).source == "3 *7")
  }

  "sourcecode.Args" in {
    def f(a: Int, b: String): Unit = {
      import sourcecode.{Args, Text}
      assert(implicitly[sourcecode.Args] == Args(List(List(
        Text(a, "a"),
        Text(b, "b")))))
    }
    f(3, "string")
  }
}
