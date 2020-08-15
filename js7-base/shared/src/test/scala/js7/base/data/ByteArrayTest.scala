package js7.base.data

final class ByteArrayTest extends ByteSequenceTester[ByteArray]
{
  "unsafeWrap, unsafeArray" in {
    val a = Array(1.toByte)
    assert(ByteArray.unsafeWrap(a).unsafeArray eq a)
  }

  "toString" in {
    assert(ByteArray.empty.toString == "ByteArray.empty")
    assert(ByteArray("123").toString == "ByteArray(313233  123)")
    assert(ByteArray("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").toString ==
      "ByteArray(61626364 65666768 696a6b6c 6d6e6f70 71727374 75767778 797a4142 43444546  abcd efgh ijkl mnop qrst uvwx yzAB CDEF)")
    assert(ByteArray("å").toString == "ByteArray(c3a5  ¿¿)")
  }
}
