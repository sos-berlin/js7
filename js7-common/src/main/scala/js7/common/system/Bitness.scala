package js7.common.system

import scala.sys.error

object Bitness extends Enumeration:
  // Wir setzen erstmal Oracles Java voraus
  // private val arch32 = Set("x86")
  // private val arch64 = Set("x64", "amd64")
  // private val osArch = Option(System.getProperty("os.arch")) getOrElse error("System property os.arch is undefined")

  private val sunModel = Option(System.getProperty("sun.arch.data.model")) getOrElse
      error("System property sun.arch.data.model is undefined")

  val Bits32 = Value
  val Bits64 = Value

  val bitness = sunModel match
    case "32" => Bits32
    case "64" => Bits64
    case _ => error(s"Unknown value in system property sun.arch.data.model=$sunModel")

  def is64Bit = bitness == Bits64
  def is32Bit = bitness == Bits32
