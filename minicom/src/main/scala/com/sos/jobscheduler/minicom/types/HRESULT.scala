package com.sos.scheduler.engine.minicom.types

/**
 * COM 32bit result and error code, like Microsoft's HRESULT.
 *
 * @author Joacim Zschimmer
 */
final case class HRESULT(value: Int) {
  def isError = value < 0
  def comString = s"COM-$toHex"
  override def toString = s"HRESULT(0x$toHex)"
  def toHex = f"$value%08X"
}

object HRESULT {
  final val S_OK                          = HRESULT(0)
//final val S_FALSE                       = HRESULT(1)
//final val ERROR                         = HRESULT(0x80000000)
//final val E_NOTIMPL                     = HRESULT(0x80004001)
//final val E_NOINTERFACE                 = HRESULT(0x80004002)
  final val E_POINTER                     = HRESULT(0x80004003)
//final val E_FAIL                        = HRESULT(0x80004005)
//final val E_UNEXPECTED                  = HRESULT(0x8000FFFF)
//final val DISP_E_UNKNOWNINTERFACE       = HRESULT(0x80020001)
  final val DISP_E_MEMBERNOTFOUND         = HRESULT(0x80020003)
  final val DISP_E_PARAMNOTFOUND          = HRESULT(0x80020004)
//final val DISP_E_TYPEMISMATCH           = HRESULT(0x80020005)
  final val DISP_E_UNKNOWNNAME            = HRESULT(0x80020006)
  final val DISP_E_BADVARTYPE             = HRESULT(0x80020008)
  final val DISP_E_EXCEPTION              = HRESULT(0x80020009)
  //final val DISP_E_BADINDEX               = HRESULT(0x8002000B)
  final val DISP_E_BADPARAMCOUNT          = HRESULT(0x8002000E)
//final val CLASS_E_CLASSNOTAVAILABLE     = HRESULT(0x80040111)
//final val REGDB_E_CLASSNOTREG           = HRESULT(0x80040154)
  final val E_INVALIDARG                  = HRESULT(0x80070057)
//final val CO_S_NOTALLINTERFACES         = HRESULT(0x00080012)
}
