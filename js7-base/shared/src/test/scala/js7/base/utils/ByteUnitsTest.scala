package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.ByteUnits.{formatNumber, toKBGB, toKiBGiB, toMB}

/**
  * @author Joacim Zschimmer
  */
final class ByteUnitsTest extends OurTestSuite
{
  "formatNumber" in {
    for (i <- 0 to 9) {
      assert(formatNumber(i, 1) == i.toString)
    }
    assert(formatNumber(10, 1) == "10")

    // Base 1000
    assert(formatNumber(1000, 1000) == "1")
    assert(formatNumber(1099, 1000) == "1")
    assert(formatNumber(1100, 1000) == "1.1")
    assert(formatNumber(1199, 1000) == "1.1")
    assert(formatNumber(1900, 1000) == "1.9")
    assert(formatNumber(1999, 1000) == "1.9")
    assert(formatNumber(2000, 1000) == "2")
    assert(formatNumber(2099, 1000) == "2")
    assert(formatNumber(2900, 1000) == "2.9")
    assert(formatNumber(2999, 1000) == "2.9")
    assert(formatNumber(9999, 1000) == "9.9")
    assert(formatNumber(10000, 1000) == "10")
    assert(formatNumber(10100, 1000) == "10")
    assert(formatNumber(11000, 1000) == "11")
    assert(formatNumber(99999, 1000) == "99")

    // Base 1024
    assert(formatNumber(1023, 1024) == "0.9")
    assert(formatNumber(1024, 1024) == "1")
    assert(formatNumber(1024+102, 1024) == "1")
    assert(formatNumber(1024+103, 1024) == "1.1")
    assert(formatNumber(1024+204, 1024) == "1.1")
    assert(formatNumber(1024+205, 1024) == "1.2")
    assert(formatNumber(2048-1, 1024) == "1.9")
    assert(formatNumber(2048, 1024) == "2")
    assert(formatNumber(10*1024-1, 1024) == "9.9")
    assert(formatNumber(10*1024, 1024) == "10")
    assert(formatNumber(11*1024-1, 1024) == "10")
    assert(formatNumber(11*1024, 1024) == "11")
  }

  "toKBGB" in {
    assert(toKBGB(-1) == "-1bytes")
    assert(toKBGB(0) == "0bytes")
    assert(toKBGB(1) == "1bytes")
    assert(toKBGB(999) == "999bytes")
    assert(toKBGB(1000) == "1kB")
    assert(toKBGB(1100) == "1.1kB")
    assert(toKBGB(1199) == "1.1kB")
    assert(toKBGB(999999) == "999kB")
    assert(toKBGB(1000000) == "1MB")
    assert(toKBGB(1100000) == "1.1MB")
    assert(toKBGB(1199999) == "1.1MB")
    assert(toKBGB(10999999) == "10MB")
    assert(toKBGB(999999999) == "999MB")
    assert(toKBGB(1000000000) == "1GB")
    assert(toKBGB(1100000000) == "1.1GB")
    assert(toKBGB(1199999999) == "1.1GB")
    assert(toKBGB(10000000000L) == "10GB")
  }

  "toMB" in {
    assert(toMB(-1) == "-1bytes")
    assert(toMB(0) == "0MB")
    assert(toMB(1) == "<1MB")
    assert(toMB(999999) == "<1MB")
    assert(toMB(1000000) == "1MB")
    assert(toMB(999999999) == "999MB")
    assert(toMB(1000000000) == "1GB")
  }

  "toKiBGiB" in {
    val K = 1024L
    val M = 1024*K
    val G = 1024*M
    assert(toKiBGiB(-1) == "-1bytes")
    assert(toKiBGiB(0) == "0bytes")
    assert(toKiBGiB(1) == "1bytes")
    assert(toKiBGiB(K-1) == "1023bytes")
    assert(toKiBGiB(K) == "1KiB")
    assert(toKiBGiB(K+K/2) == "1.5KiB")
    assert(toKiBGiB(M-1) == "1023KiB")
    assert(toKiBGiB(M) == "1MiB")
    assert(toKiBGiB(M+M/2) == "1.5MiB")
    assert(toKiBGiB(M*K-1) == "1023MiB")
    assert(toKiBGiB(G+G/2) == "1.5GiB")
    assert(toKiBGiB(10*G+G/2) == "10GiB")
    assert(toKiBGiB(1024*G) == "1024GiB")
  }
}
