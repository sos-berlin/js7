package js7.base.log.reader

import cats.effect.{IO, Resource, ResourceIO}
import fs2.Chunk
import js7.base.io.OpaquePos

/** Allows writing a compressed file and marks seekable positions in this file.
  *
  * For easier handling, despite I/O, the methods don't return IO.
  * The calls are expected to be fast and done only during indexing.
  */
trait LogWriter:
  def write(chunk: Chunk[Byte]): Unit

  /** The byte position, counted in bytes written with `write`. */
  def position: Long

  /** Mark the current position as positionable and return the OpaquePos.
    *
    * A simple file would simply return the written byte count (see also the [[Void]] subclass)
    *
    * For a compress file, the compression would be finished.
    * The following data must be decompressable without knowledge of the forgoing data.
    *
    * @return The position in the (maybe) compressed file.
    */
  def markOpaquePos(): OpaquePos


object LogWriter:


  final class Void(startPosition: Long = 0) extends LogWriter:
    private var _position = startPosition

    def write(chunk: Chunk[Byte]) =
      _position += chunk.size

    def position =
      _position

    def markOpaquePos() =
      OpaquePos(_position)

  object Void:
    def resource(startPosition: Long = 0): ResourceIO[Void] =
      Resource.eval(IO(LogWriter.Void(startPosition)))
