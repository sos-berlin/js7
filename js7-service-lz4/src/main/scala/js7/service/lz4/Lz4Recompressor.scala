package js7.service.lz4

import java.io.{InputStream, OutputStream}
import js7.base.io.file.ByteSeqFileReader
import js7.base.io.{SeekableInputStream, SeekableOutputStream}
import js7.base.log.Logger
import js7.base.log.reader.recompressors.SeekableInputStreamRecompressor
import js7.base.utils.Tests.isTest
import js7.service.lz4.Lz4InputStream
import js7.service.lz4.Lz4Recompressor.*
import net.jpountz.lz4.{LZ4Compressor, LZ4Factory, LZ4FastDecompressor}

final class Lz4Recompressor(
  makeCompressor: () => LZ4Compressor,
  makeDecompressor: () => LZ4FastDecompressor,
  name: String)
extends
  SeekableInputStreamRecompressor:

  def this() = this(
    () => LZ4Factory.fastestJavaInstance.fastCompressor(),
    () => LZ4Factory.fastestJavaInstance.fastDecompressor(),
    "lz4")

  private val logger = Logger[this.type]
  private val BufferSize = ByteSeqFileReader.BufferSize + 1024

  logger.info("Using LZ4 compression")

  def findRecompressor(name: String) =
    name match
      case "lz4" => Some(if isTest then this else FastestLz4)
      case "lz4/java" => Some(this)
      case "lz4/fastest" => Some(FastestLz4)
      case _ => None

  protected def newCompressiongOutputStream(out: OutputStream): SeekableOutputStream =
    Lz4OutputStream(out, makeCompressor, bufferSize = BufferSize)

  def decompressingInputStream(in: InputStream): SeekableInputStream =
    Lz4InputStream(in, makeDecompressor, bufferSize = BufferSize)

  override def toString = "Lz4Recompressor"


private object Lz4Recompressor:
  // Don't use FastestLz4 in tests
  private val FastestLz4 = Lz4Recompressor(
    () => LZ4Factory.fastestInstance.fastCompressor(),
    () => LZ4Factory.fastestInstance.fastDecompressor(),
    "lz4/native")
