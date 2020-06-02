package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.common.utils.ByteUnits.{toKBGB, toKiBGiB, toMB}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ByteUnitsTest extends AnyFreeSpec {

  "toKBGB" in {
    assert(toKBGB(-1) == "-1bytes")
    assert(toKBGB(0) == "0kB")
    assert(toKBGB(1) == "<1kB")
    assert(toKBGB(999) == "<1kB")
    assert(toKBGB(1000) == "1kB")
    assert(toKBGB(999999) == "999kB")
    assert(toKBGB(1000000) == "1MB")
    assert(toKBGB(999999999) == "999MB")
    assert(toKBGB(1000000000) == "1GB")
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
    assert(toKiBGiB(-1) == "-1bytes")
    assert(toKiBGiB(0) == "0KiB")
    assert(toKiBGiB(1) == "<1KiB")
    assert(toKiBGiB(1024-1) == "<1KiB")
    assert(toKiBGiB(1024) == "1KiB")
    assert(toKiBGiB(1024*1024-1) == "1023KiB")
    assert(toKiBGiB(1024*1024) == "1MiB")
    assert(toKiBGiB(1024*1024*1024-1) == "1023MiB")
    assert(toKiBGiB(1024*1024*1024) == "1GiB")
    assert(toKiBGiB(1024L*1024*1024*1024) == "1024GiB")
  }
}
