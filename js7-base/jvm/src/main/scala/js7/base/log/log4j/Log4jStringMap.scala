package js7.base.log.log4j

import js7.base.log.CorrelId
import js7.base.log.log4j.Log4jStringMap.*
import js7.base.log.log4j.Log4jThreadContextMap.CorrelIdKey
import js7.base.utils.Tests.isTest
import org.apache.logging.log4j.util.{BiConsumer, ReadOnlyStringMap, StringMap, TriConsumer}

final class Log4jStringMap(private[log] val correlId: CorrelId)
extends StringMap:

  private def correlIdString =
    if correlId != null.asInstanceOf[CorrelId] then
      correlId.fixedWidthString
    else if isTest then
      throw new NullPointerException("Log4jStringMap: correlId is null") // ???
    else
      nullString

  def toMap: java.util.Map[String, String] =
   toJavaMap

  private lazy val toJavaMap: java.util.Map[String, String] =
    java.util.Collections.singletonMap(CorrelIdKey, correlIdString)

  def isEmpty = false

  def size = 1

  def isFrozen = true

  def freeze(): Unit = {}

  def containsKey(key: String): Boolean =
    key == CorrelIdKey

  def getValue[V](key: String): V =
    val string: String | Null =
      if key == CorrelIdKey then
        CorrelId.onCorrelIdLogged()
        correlIdString
      else
        Log4jThreadContextMap.getOtherKey(key)
    string.asInstanceOf[V]

  def forEach[V](action: BiConsumer[String, ? >: V]): Unit =
    assert(!isTest)
    action.accept(CorrelIdKey, correlIdString.asInstanceOf[V])

  def forEach[V, S](action: TriConsumer[String, ? >: V, S], state: S): Unit =
    assert(!isTest)
    _forEachCount += 1
    action.accept(CorrelIdKey, correlIdString.asInstanceOf[V], state)

  def clear(): Nothing =
    assert(!isTest)
    throwFrozen()

  def putAll(source: ReadOnlyStringMap): Nothing =
    throwFrozen()

  def putValue(key: String, value: Any): Nothing =
    throwFrozen()

  def remove(key: String): Nothing =
    throwFrozen()

  private def throwFrozen(): Nothing =
    throw new IllegalStateException("CorrelId StringMap is frozen")


object Log4jStringMap:
  private val nullString = "❓null❓"
  private var _forEachCount = 0L

  def forEachCount: Long =
    _forEachCount
