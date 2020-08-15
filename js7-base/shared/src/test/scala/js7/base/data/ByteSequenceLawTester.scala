package js7.base.data

import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.{AnyFunSuite, AnyFunSuiteLike}
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.{Discipline, FunSuiteDiscipline}

class ByteSequenceLawTester[ByteSeq](implicit ByteSeq: ByteSequence[ByteSeq])
extends AnyFunSuite with FunSuiteDiscipline with Configuration
{
  private implicit def arbByteSeq: Arbitrary[ByteSeq] =
    Arbitrary(Gen.oneOf(Gen.const(ByteSeq.empty), Arbitrary.arbitrary[String] map ByteSeq.fromString))

  checkAll("ByteSequenceLaws", MonoidTests[ByteSeq].monoid)
}
