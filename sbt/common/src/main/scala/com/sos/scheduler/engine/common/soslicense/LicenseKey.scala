package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.soslicense.LicenseKey._
import com.sos.scheduler.engine.common.soslicense.Parameters._
import com.sos.scheduler.engine.common.soslicense.UnsignedInt32._
import java.time.LocalDate
import java.time.format.DateTimeFormatter.BASIC_ISO_DATE
import scala.util.control.NonFatal

/**
 * Validated SOS license key.
 * Migrated from C++ licence.cxx.
 *
 * @author Joacim Zschimmer
 */
case class LicenseKey private(
  final val keyString: LicenseKeyString,
  final val issuer: String,
  final val customer: String,
  final val serialNumber: Int,
  final val issuedAt: java.time.LocalDate,
  final val settings: Map[Parameter, String],
  final val securityCode: Int,
  final val salt: Int)
extends LicenseKeyChecker {

  val validUntilOption: Option[LocalDate] = {
    def v(p: Parameter) = settings.get(p) map { p.string + _ }
    v(ValidIn1900) orElse v(ValidIn2000) map { o ⇒ LocalDate from BASIC_ISO_DATE.parse(o) }
  }

  def apply(parameter: Parameter): Parameter.Result =
    if (!contains(parameter)) Parameter.Missing
    else
    if (!isValidToday) Parameter.Expired
    else Parameter.OK

  private def contains(p: Parameter) = (settings contains p) || containsUniversally(p)

  private def containsUniversally(p: Parameter) = (settings contains ZZ) && Parameters.ZZIncludes(p)

  private[soslicense] final def isValidToday = validUntilOption map { o => !(LocalDate.now() isAfter o) } getOrElse true
}

object LicenseKey {
  private val logger = Logger(getClass)
  private val Base = 36
  val Empty = new LicenseKey(
    keyString = LicenseKeyString("empty-license"),
    issuer = "SOS",
    customer = "EMPTY",
    serialNumber = 0,
    issuedAt = LocalDate.of(2015, 8, 24),
    settings = Map(),
    securityCode = 0,
    salt = 0)

  def apply(key: String): LicenseKey = apply(LicenseKeyString(key))
  
  def apply(key: LicenseKeyString): LicenseKey =
    try {
      val parts = key.normalizedString split '-'
      val mainParts = parts dropRight 1
      val securityPart = parts.last
      require(securityPart.length == 7)
      val issuer = parts(0)
      val customer = parts(1)
      val serialNumber = parts(2).toInt
      val issuedAt = {
        val s = parts(3)
        require(s.length == 3)
        LocalDate.of(1990 + charToInt(s(0)), charToInt(s(1)), charToInt(s(2)))
      }
      val settings = for (i ← 4 until parts.length - 1) yield parseSetting(parts(i))
      val securityCode = stringToUnsignedInt(securityPart.tail)
      val salt = securityPart.head
      val calculatedSecurityCode = {
        val b = new SecurityCodeBuilder
        for (_ ← 1 to 7) {
          b.addString(issuer)
          b.addString(customer)
          b.addByteIfNotZero((serialNumber >> 24) & 0xFF)
          b.addByteIfNotZero((serialNumber >> 16) & 0xFF)
          b.addByteIfNotZero((serialNumber >> 8) & 0xFF)
          b.addByteIfNotZero(serialNumber & 0xFF)
          b.addByte(issuedAt.getYear & 0xFF)
          b.addByte(issuedAt.getMonthValue)
          b.addByte(issuedAt.getDayOfMonth)
          for ((par, value) ← settings if par != ZZ) {
            if (par.toInt >= 0x100) b.addByte(par.toInt >> 8)
            b.addByte(par.toInt & 0xFF)
            b.addString(value)
          }
          if (settings exists { _._1 == ZZ }) {
            b.addByte(35)
            b.addByte(35)
          }
          b.addByte(salt)
        }
        b.securityCode % ((Base.unsigned * Base * Base * Base * Base * Base) - 1.unsigned)  // 36**6 - 1
      }
      if (securityCode != calculatedSecurityCode) throw new InvalidLicenseKeyException(key)
      new LicenseKey(key, issuer, customer, serialNumber, issuedAt, settings.toMap, securityCode.toInt, salt) {
        override def toString = mainParts mkString "-"
      }
    }
    catch {
      case NonFatal(t) ⇒
        logger.debug(s"Invalid license key: $key ($t)")
        throw new InvalidLicenseKeyException(key)
    }

  private def parseSetting(string: String): (Parameter, String) =
    if (string.length == 1)
      Parameter(LicenseKeyString.normalize(string)) → ""
    else
      Parameter(string take 2) → (string drop 2)

  final case class Parameter(string: String) {
    def toInt =
      string.length match {
        case 1 ⇒ charToInt(string.head)
        case 2 ⇒ charToInt(string(0)) * Base + charToInt(string(1))
      }
  }

  object Parameter {
    sealed trait Result
    case object OK extends Result
    case object Missing extends Result
    case object Expired extends Result
  }

  private def stringToUnsignedInt(string: String): UnsignedInt32 = {
    var r = 0.unsigned
    var b = 1.unsigned
    for (c ← string) {
      r += b * charToInt(c)
      b *= Base
    }
    r
  }

  private def charToInt(c: Char): Int =
    c match {
      case _ if c >= '0' && c <= '9' ⇒ c - '0'
      case _ if c >= 'A' && c <= 'Z' ⇒ 10 + c - 'A'
    }

  private class SecurityCodeBuilder {
    private var hash = 0.unsigned

    def addString(string: String): Unit = for (c ← string) addByte(c)

    def addByteIfNotZero(byte: Int): Unit = if (byte != 0) addByte(byte)

    def addByte(byte: Int): Unit =
      hash = (((hash % 0x7efefefd) << 1) | (hash >> (32 - 1))) ^
        (((byte & 0xFF) + (if ((hash & 0x200) != 0.unsigned) 0x6A else 0xA6)) & 0xff)

    def securityCode = hash
  }
}
