package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.base.utils.HasKey
import com.sos.scheduler.engine.common.scalautil.DuplicateKeyException
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ConcurrentRegisterTest extends FreeSpec {

  private val register = ConcurrentRegister[Value]()

  "Initially, ConcurrentRegister is empty" in {
    assert(register.isEmpty)
    assert(!register.nonEmpty)
    assert(register.size == 0)
    assert((register map { _.key }) == Nil)
    assert(register.totalCount == 0)
  }

  "Add a first entry" in {
    register.insert(Value(1))
    assert(!register.isEmpty)
    assert(register.nonEmpty)
    assert(register.size == 1)
    assert((register map { _.key }) == List(1))
    assert(register.totalCount == 1)
    assert(register(1) == Value(1))
  }

  "Add a second entry" in {
    register.insert(Value(2))
    assert(!register.isEmpty)
    assert(register.nonEmpty)
    assert(register.size == 2)
    assert((register map { _.key }) == List(1, 2))
    assert(register.totalCount == 2)
    var values = mutable.Set[Value]()
    for (v ← register) values += v
    assert(values == Set(Value(1), Value(2)))
  }

  "Unknown key" in {
    intercept[NoSuchElementException] { register(99) }
  }

  "get" in {
    assert(register.get(1) == Some(Value(1)))
    assert(register.get(99) == None)
  }

  "getOrElse" in {
    assert(register.getOrElse(1, Value(99)) == Value(1))
    assert(register.getOrElse(9, Value(99)) == Value(99))
  }

  "Adding a known entry is rejected" in {
    intercept[DuplicateKeyException] {
      register.insert(Value(2))
    }
    intercept[DuplicateKeyException] {
      register.insert(Value(2))
    }
  }

  "Remove first entry" in {
    register -= 1
    assert(!register.isEmpty)
    assert(register.nonEmpty)
    assert(register.size == 1)
    assert((register map { _.key }) == List(2))
    assert(register.totalCount == 2)
    var values = mutable.Set[Value]()
    for (v ← register) values += v
    assert(values == Set(Value(2)))
  }

  "Remove second entry" in {
    register -= 2
    assert(register.isEmpty)
    assert(!register.nonEmpty)
    assert(register.size == 0)
    assert((register map { _.key }) == Nil)
    assert(register.totalCount == 2)
  }

  private case class Value(key: Int) extends HasKey {
    type Key = Int
  }
}
