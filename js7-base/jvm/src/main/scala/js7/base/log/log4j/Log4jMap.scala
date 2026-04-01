package js7.base.log.log4j

import java.util
import java.util.Map
import js7.base.log.CorrelId
import js7.base.log.log4j.Log4jMap.*
import js7.base.log.log4j.Log4jThreadContextMap.CorrelIdKey
import js7.base.utils.Tests.isTest

final class Log4jMap(private[log] val correlId: CorrelId)
extends java.util.Map[String, String]:

  def isEmpty =
    false

  def size =
    1 + Log4jThreadContextMap.keyToValue.size

  def containsKey(key: Any): Boolean =
    key == CorrelIdKey || Log4jThreadContextMap.keyToValue.containsKey(key)

  def containsValue(value: Any): Boolean =
    throw new UnsupportedOperationException("containsValue")

  def get(key: Any): String | Null =
    _getCount += 1
    key match
      case CorrelIdKey =>
        CorrelId.onCorrelIdLogged()
        correlIdString
      case key: String =>
        Log4jThreadContextMap.getOtherKey(key)
      case _ =>
        null

  private def correlIdString =
    if correlId != null.asInstanceOf[CorrelId] then
      correlId.fixedWidthString
    else if isTest then
      throw new NullPointerException("Log4jMap: correlId is null") // ???
    else
      nullString

  def values(): java.util.Collection[String] =
    throw new UnsupportedOperationException("values")

  def keySet(): java.util.Set[String] =
    throw new UnsupportedOperationException("keySet")

  def entrySet(): util.Set[Map.Entry[String, String]] =
    throw new UnsupportedOperationException("entrySet")

  def clear() =
    throwImmutable()

  def remove(key: Any): String =
    throwImmutable()

  def put(key: String, value: String): String =
    throwImmutable()

  def putAll(map: java.util.Map[? <: String, ? <: String]): Unit =
    throwImmutable()

  private def throwImmutable(): Nothing =
    assert(!isTest)
    throw new IllegalStateException("Log4jMap is immutable")


object Log4jMap:
  private val nullString = "❓null❓"
  private var _getCount = 0L

  def getCount: Long =
    _getCount
