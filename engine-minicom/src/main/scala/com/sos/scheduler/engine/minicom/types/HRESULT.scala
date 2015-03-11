package com.sos.scheduler.engine.minicom.types

/**
 * COM 32bit error code.
 * @author Joacim Zschimmer
 */
final case class HRESULT(value: Int) {
  def isError = value < 0
  def comString = f"COM-$value%08X"
  override def toString = f"HRESULT(0x$value%08X)"
}

object HRESULT {
  final val S_OK                          = HRESULT(0)
  //final val S_FALSE                       = HRESULT(1)
  //
  //final val ERROR                         = HRESULT(0x80000000)
  //
  //final val E_NOTIMPL                     = HRESULT(0x80004001)
  //final val E_NOINTERFACE                 = HRESULT(0x80004002)
  final val E_POINTER                     = HRESULT(0x80004003)
  //final val E_ABORT                       = HRESULT(0x80004004)
  //final val E_FAIL                        = HRESULT(0x80004005)
  //final val E_UNEXPECTED                  = HRESULT(0x8000FFFF)
  //
  //final val DISP_E_UNKNOWNINTERFACE       = HRESULT(0x80020001)
  final val DISP_E_MEMBERNOTFOUND         = HRESULT(0x80020003)
  final val DISP_E_PARAMNOTFOUND          = HRESULT(0x80020004)
  //final val DISP_E_TYPEMISMATCH           = HRESULT(0x80020005)
  final val DISP_E_UNKNOWNNAME            = HRESULT(0x80020006)
  //final val DISP_E_NONAMEDARGS            = HRESULT(0x80020007)
  final val DISP_E_BADVARTYPE             = HRESULT(0x80020008)
  final val DISP_E_EXCEPTION              = HRESULT(0x80020009)
  //final val DISP_E_OVERFLOW               = HRESULT(0x8002000A)
  //
  //final val DISP_E_BADINDEX               = HRESULT(0x8002000B)
  //
  final val DISP_E_BADPARAMCOUNT          = HRESULT(0x8002000E)
  //
  //final val TYPE_E_ELEMENTNOTFOUND        = HRESULT(0x8002802B)
  //
  //final val CLASS_E_NOTLICENSED           = HRESULT(0x80040112)
  //final val CLASS_E_CLASSNOTAVAILABLE     = HRESULT(0x80040111)
  //final val CLASS_E_NOAGGREGATION         = HRESULT(0x80040110)
  //
  //final val REGDB_E_WRITEREGDB            = HRESULT(0x80040151)
  //final val REGDB_E_CLASSNOTREG           = HRESULT(0x80040154)
  //
  //final val CO_E_NOTINITIALIZED           = HRESULT(0x800401F0)
  //final val CO_E_DLLNOTFOUND              = HRESULT(0x800401F8)
  //final val CO_E_ERRORINDLL               = HRESULT(0x800401F9)
  //
  //final val E_ACCESSDENIED                = HRESULT(0x80070005)
  //final val E_HANDLE                      = HRESULT(0x80070006)
  //final val E_OUTOFMEMORY                 = HRESULT(0x8007000E)
  final val E_INVALIDARG                  = HRESULT(0x80070057)
  //
  //final val CO_S_NOTALLINTERFACES         = HRESULT(0x00080012)
}
