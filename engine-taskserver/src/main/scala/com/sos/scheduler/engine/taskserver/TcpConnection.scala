package com.sos.scheduler.engine.taskserver

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.minicom.remoting.MessageConnection
import com.sos.scheduler.engine.taskserver.TcpConnection._
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousCloseException, SocketChannel}
import scala.concurrent.blocking

final class TcpConnection(peerAddress: InetSocketAddress) extends MessageConnection with HasCloser {

  private val channel = SocketChannel.open().closeWithCloser

  def connect(): Unit = {
    blocking {
      logger.debug(s"Connecting with $peerAddress ...")
      channel.connect(peerAddress)
      logger.debug(s"Connected with $peerAddress")
      assert(channel.isBlocking)
    }
  }

  /**
   * @return None: Connection has been closed before next message
   */
  def receiveMessage(): Option[ByteBuffer] = {
    val lengthBuffer = ByteBuffer.allocate(4)
    receiveBuffer(lengthBuffer)
    if (lengthBuffer.position == 0)
      None
    else {
      lengthBuffer.rewind()
      val buffer = ByteBuffer.allocate(lengthBuffer.getInt)
      receiveBuffer(buffer)
      if (buffer.position != buffer.limit) throw new AsynchronousCloseException
      buffer.rewind()
      Some(buffer)
    }
  }

  private def receiveBuffer(buffer: ByteBuffer) = {
    do blocking { channel.read(buffer) } while (buffer.position > 0 && buffer.position < buffer.limit)
    assert(buffer.position == 0 || buffer.position == buffer.limit)
  }

  def sendMessage(data: ByteString): Unit = sendMessage(data.asByteBuffers, data.size)

  def sendMessage(data: Array[Byte], length: Int): Unit = sendMessage(List(ByteBuffer.wrap(data, 0, length)), length)

  private def sendMessage(byteBuffers: Iterable[ByteBuffer], size: Int): Unit = {
    val lengthBuffer = ByteBuffer.allocate(4)
    lengthBuffer.putInt(size)
    lengthBuffer.flip()
    blocking {
      // Send as one TCP packet, with one write
      channel.write(Array(lengthBuffer) ++ byteBuffers)
    }
  }

  override def toString = s"TcpConnection($peerAddress)"
}

object TcpConnection{
  private val logger = Logger(getClass)
}
