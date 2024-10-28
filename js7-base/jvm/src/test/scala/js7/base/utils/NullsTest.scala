package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.Nulls.notNullOr

final class NullsTest extends OurTestSuite:

  "notNullOr" in:
    locally:
      val a: 7 | Null = 7
      val b: Int = a.notNullOr(9)
      assert(b == 7)

    locally:
      val a: 7 | Null = 7
      val b: Int | String = a.notNullOr("NULL")
      assert(b == 7)

    locally:
      val a: 7 | Null = null
      val b: Int = a.notNullOr(9)
      assert(b == 9)

    locally:
      val a: 7 | Null = null
      val b: Int | String = a.notNullOr("NULL")
      assert(b == "NULL")

    locally:
      val a: 7 | Null = 7
      val b: Int = a.notNullOr(throw new AssertionError)
      assert(b == 7)

  "NonNull" - {
    "NonNull(nonNull)" in:
      val a: String | Null = "A"
      a match
        case NonNull(a) => assert((a: String) == "A")
        case _ => fail()

    "NonNull(null)" in:
      val a: String | Null = null
      (a: @unchecked) match
        // "match might not exhaustive" due to NonNull
        case NonNull(_) => fail()
        case null => succeed
  }

  //"IsNull" - {
  //  "IsNull(7: Int | Null)" in:
  //    val a: Int | Null = 7
  //    a match
  //      // "match might not exhaustive" due to IsNull
  //      case IsNull(()) => fail()
  //      case a: Int => assert(a == 7)
  //
  //  "IsNull(null: Int | Null)" in:
  //    val a: Int | Null = null
  //    a match
  //      case IsNull(()) => succeed
  //      case _ => fail()
  //
  //  "IsNull(7)" in:
  //    val a: Int | Null = 7
  //    a match
  //      // "match might not exhaustive" due to IsNull
  //      case IsNull(a) => fail()
  //      case a: Int => assert(a == 7)
  //
  //  "IsNull(7: Int)" in:
  //    val a: Integer = null.asInstanceOf[Integer]
  //    a match
  //      case IsNull(()) => succeed
  //      case _ => fail()
  //}
