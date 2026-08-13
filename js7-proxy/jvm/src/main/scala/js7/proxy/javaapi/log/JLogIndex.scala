package js7.proxy.javaapi.log

import java.time.Instant
import java.util.List as JList
import js7.base.log.reader.{KeyedByteLogLine, KeyedLogLine, LogLineKey}
import reactor.core.publisher.Flux

trait JLogIndex:

  /** Read chunks of raw log lines as Array[Byte] beginning with `begin`.
    *
    * Use this method when String-conversion is not needed,
    * for example when downloading to a file. */
  def byteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[Array[Byte]]]

  /** Read chunks of raw log lines as Array[Byte] beginning with `begin`.
    *
    * Use this method when String-conversion is not needed,
    * for example when downloading to a file. */
  def byteLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[Array[Byte]]]

  /** Read chunks String log lines beginning with `begin`.
    *
    * Use this method when you want String-based lines. */
  def stringLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[String]]

  /** Read chunks of byte-based KeyedByteLogLine beginning with `begin`.
    *
    * Use this method when you want byte-based KeyedByteLogLine lines annotated with `LogLineKey`.
    */
  def keyedByteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[KeyedByteLogLine]]

  /** Read chunks of byte-based KeyedByteLogLine beginning with `begin`.
    *
    * Use this method when you want byte-based KeyedByteLogLine lines annotated with `LogLineKey`. */
  def keyedByteLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedByteLogLine]]

  /** Read chunks String-based KeyedLogLine log lines beginning with `begin`.
    *
    * Use this method when you want String-based KeyedLogLine lines annotated with `LogLineKey`. */
  def keyedLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[KeyedLogLine]]

  /** Read chunks of String-based KeyedLogLine beginning with `begin`.
    *
    * Use this method when you want String-based KeyedLogLine lines annotated with `LogLineKey`. */
  def keyedLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedLogLine]]
