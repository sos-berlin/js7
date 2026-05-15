package js7.base.io

/** A position in a possibly compressed file.
  *
  * If the file is compressed, then a OpaquePos is different from the uncompressed byte position.
  *
  * In a compressed file, only selected positions (OpaquePos) are allowed because
  * each such OpaquePos must point to an independently compressed chunk.
  *
  * See EpochNanoToPos for a conversion index between byte position and OpaquePos
  */
opaque type OpaquePos = Long

object OpaquePos:
  inline def apply(inline pos: Long): OpaquePos =
    pos

  extension (opaquePos: OpaquePos)
    inline def toLong: Long =
      opaquePos
