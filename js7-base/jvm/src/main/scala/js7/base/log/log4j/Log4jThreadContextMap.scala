package js7.base.log.log4j

import java.util.concurrent.ConcurrentHashMap
import js7.base.BuildInfo
import js7.base.log.log4j.Log4jThreadContextMap.*
import js7.base.system.startup.StartUp
import js7.base.utils.Lazy
import org.apache.logging.log4j.spi.{DefaultThreadContextMap, ReadOnlyThreadContextMap}
import org.apache.logging.log4j.util.{SortedArrayStringMap, StringMap}
import scala.jdk.CollectionConverters.*

final class Log4jThreadContextMap extends DefaultThreadContextMap, ReadOnlyThreadContextMap:

  def getReadOnlyContextData: StringMap =
    asStringMap


object Log4jThreadContextMap:
  private val MyClassName = classOf[Log4jThreadContextMap].getName.stripSuffix("$")

  private[log] val keyToValue = new ConcurrentHashMap[String, String | Lazy[String]]:
    this.put("js7.version", BuildInfo.longVersion)
    this.put("js7.longVersion", BuildInfo.longVersion)
    this.put("js7.prettyVersion", BuildInfo.prettyVersion)
    this.put("js7.system", Lazy.fast(StartUp.startUpLine))
    //this.put(CorrelIdKey, "❓init❓") // Placeholder in SortedArrayStringMap for fast override

  private var keyToValueVersion = 0
  private var _stringMap: StringMap = null.asInstanceOf[StringMap]
  private var _stringMapVersion = -1

  def initialize(name: String): Unit =
    keyToValue.put("js7.serverId", name) // May be overwritten later by a more specific value
    System.setProperty("log4j2.threadContextMap", MyClassName)

  private[log] def put(key: String, value: String): Unit =
    keyToValue.put(key, value)
    keyToValueVersion += 1

  private inline def asStringMap: StringMap =
    if _stringMapVersion != keyToValueVersion then
      // SortedArrayStringMap is fast when it is merged with another SortedArrayStringMap.
      _stringMap = SortedArrayStringMap:
        keyToValue.asScala.view.mapValues(resolveValue).toMap.asJava
      _stringMapVersion = keyToValueVersion
    _stringMap

  private def resolveValue(value: String | Null | Lazy[String]): String | Null =
    value match
      case lzy: Lazy[String] => lzy.value
      case o => o.asInstanceOf[String]

  def logStatistics(): Unit =
    ()
