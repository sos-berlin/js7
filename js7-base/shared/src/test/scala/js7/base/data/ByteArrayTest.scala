package js7.base.data

import js7.base.data.ByteSequence.ops._

final class ByteArrayTest extends ByteSequenceTester[ByteArray]
{
  "unsafeWrap, unsafeArray" in {
    val a = Array(1.toByte)
    assert(ByteArray.unsafeWrap(a).unsafeArray eq a)
  }

  "++" in {
    assert(ByteArray.empty ++ ByteArray.empty == ByteArray.empty)
    assert(ByteArray.empty ++ ByteArray(1) == ByteArray(1))
    assert(ByteArray(1) ++ ByteArray.empty == ByteArray(1))
    assert(ByteArray(1) ++ ByteArray(2, 3) == ByteArray(1, 2, 3))
    assert(ByteArray(1, 2) ++ ByteArray(3) == ByteArray(1, 2, 3))
  }

  "toByteArray eq" in {
    val a = ByteArray(1.toByte, 2.toByte)
    assert(a.toByteArray eq a)
  }

  "toString" in {
    assert(ByteArray.empty.toString == "ByteArray.empty")
    assert(ByteArray("ABC").toString == "ByteArray(length=3 »ABC« 414243)")
    assert(ByteArray("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").toString ==
      "ByteArray(length=52 »abcdefgh ijklmnop qrstuvwx yzABCDEF« 61626364 65666768 696a6b6c 6d6e6f70 71727374 75767778 797a4142 43444546)")
    assert(ByteArray("å").toString == "ByteArray(length=2 »¿¿« c3a5)")
  }

  "toStringWithHex" in {
    assert(ByteArray("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").toStringWithHex ==
      "ByteArray(length=52 »abcdefgh ijklmnop qrstuvwx yzABCDEF GHIJKLMN OPQRSTUV WXYZ« " +
        "61626364 65666768 696a6b6c 6d6e6f70 71727374 75767778 797a4142 43444546 4748494a 4b4c4d4e 4f505152 53545556 5758595a)")
  }
}
