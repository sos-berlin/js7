package js7.base.utils

import cats.effect.IO
import izumi.reflect.Tag
import js7.base.utils.IzumiTagTest.*
import js7.base.test.OurTestSuite

final class IzumiTagTest extends OurTestSuite:

  "Tag.tag" in:
    val tag = summon[Tag[MyType]]
    assert(tag.tag.toString == "IzumiTagTest$::MyTrait[=λ %A → IO[+A],=Tuple2[=Int,=Boolean]]")
    assert(tag.tag.repr == "js7.base.utils.IzumiTagTest$::MyTrait[=λ %A → cats.effect.IO[+A],=scala.Tuple2[=scala.Int,=scala.Boolean]]")


object IzumiTagTest:

  trait MyTrait[F[_], A]
  type MyType = MyTrait[IO, (Int, Boolean)]
  final class MyClass extends MyTrait[IO, (Int, Boolean)]
