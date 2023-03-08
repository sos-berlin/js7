package js7.common.utils

import js7.base.utils.Assertions.assertThat
import js7.common.utils.SimpleIterator.*

// Copied from Google Guava,
// https://raw.githubusercontent.com/google/guava/master/guava/src/com/google/common/collect/GuavaAbstractIterator.java
/*
 * Copyright (C) 2007 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

trait SimpleIterator[T] extends Iterator[T] {

  private var state: State = NOT_READY
  private var _next: T = null.asInstanceOf[T]

  /** Returns the next element. <b>Note:</b> the implementation must call `endOfData` when
   * there are no elements left in the iteration. Failure to do so could result in an infinite loop.
   *
   * <p>The initial invocation of `hasNext` or `next` calls this method, as does
   * the first invocation of `hasNext` or `next` following each successful call to
   * `next` Once the implementation either invokes `ndOfData` or throws an exception,
   * `computeNext` is guaranteed to never be called again.
   *
   * <p>If this method throws an exception, it will propagate outward to the `hasNext` or
   * `next` invcation that invoked this method. Any further attempts to use the iterator will
   * result in an `IllegalStateException`.
   *
   * <p>The implementation of this method may not invoke the `hasNext`, `next` or
   * `peek` methods on this instance; if it does, an `IllegalStateException` will result.
   *
   * @return the next element if there was one. If `endOfData` was called during execution,
   *         the return value will be ignored.
   * @throws RuntimeException if any unrecoverable error happens. This exception will propagate
   *                          outward to the `hasNext`, `next`, or `peek` inocation that invoked
   *                          this method. Any further attempts to use the iterator will result in an
   *                          `IllegalStateException`.
   */
  protected def computeNext(): T

  /** Implementations of `computeNext` <b>must</b> invoke this method when there are no
   * elements left in the iteration.
   *
   * @return `null` a convenience so your `computeNext` implementation can return `endOfData`.
   */
  final protected def endOfData: T = {
    state = DONE
    null.asInstanceOf[T]
  }

  final def hasNext: Boolean = {
    assertThat(state ne FAILED)
    state match {
      case DONE => false
      case READY =>  true
      case _ => tryToComputeNext()
    }
  }

  private def tryToComputeNext(): Boolean = {
    state = FAILED // temporary pessimism
    _next = computeNext()
    (state ne DONE) && {
      state = READY
      true
    }
  }

  final def next(): T =  {
    ensureNext()
    state = NOT_READY
    val result = _next
    _next = null.asInstanceOf[T]
    result
  }

  /** Returns the next element in the iteration without advancing the iteration.
   */
  final def peek: T = {
    ensureNext()
    _next
  }

  private def ensureNext(): Unit =
    if (!hasNext) throw new NoSuchElementException
}

object SimpleIterator {

  private sealed trait State

  /** We have computed the next element and haven't returned it yet. */
  private case object READY extends State

  /** We haven't yet computed or have already returned the element. */
  private case object NOT_READY extends State

  /** We have reached the end of the data and are finished. */
  private case object DONE extends State

  /** We've suffered an exception and are kaput. */
  private case object FAILED extends State
}
