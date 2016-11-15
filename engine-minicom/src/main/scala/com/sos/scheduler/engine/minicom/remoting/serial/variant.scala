package com.sos.scheduler.engine.minicom.remoting.serial

/**
 * @author Joacim Zschimmer
 */
private[serial] object variantTypes {
  val VT_EMPTY            = 0
  val VT_NULL             = 1
  val VT_I2               = 2
  val VT_I4               = 3
  val VT_R4               = 4
  val VT_R8               = 5
  val VT_CY               = 6
  val VT_DATE             = 7
  val VT_BSTR             = 8
  val VT_DISPATCH         = 9
  val VT_ERROR            = 10
  val VT_BOOL             = 11
  val VT_VARIANT          = 12
  val VT_UNKNOWN          = 13
  val VT_DECIMAL          = 14
  val VT_I1               = 16
  val VT_UI1              = 17
  val VT_UI2              = 18
  val VT_UI4              = 19
  val VT_I8               = 20
  val VT_UI8              = 21
  val VT_INT              = 22
  val VT_UINT             = 23
  val VT_SAFEARRAY        = 27
  val VT_ARRAY            = 0x2000
}


object variantArrayFlags {
  private[minicom] final val FADF_FIXEDSIZE  : Short =  0x10
  private[minicom] final val FADF_HAVEVARTYPE: Short =  0x80
  private[minicom] final val FADF_BSTR       : Short = 0x100
  private[minicom] final val FADF_VARIANT    : Short = 0x800
}
