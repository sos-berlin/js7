package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.remoting.MessageConnection
import com.sos.scheduler.engine.taskserver.TcpConnection._
import java.net.{InetSocketAddress, Socket}
import java.nio.ByteBuffer
import scala.concurrent.blocking

final class TcpConnection(peerAddress: InetSocketAddress) extends MessageConnection with AutoCloseable {

  private val socket = new Socket()
  private lazy val in = socket.getInputStream
  private lazy val out = socket.getOutputStream

  def connect(): Unit = {
    blocking {
      logger.debug(s"Connecting with $peerAddress ...")
      socket.connect(peerAddress)
      logger.debug(s"Connected with $peerAddress")
    }
  }

  def close(): Unit = socket.close()

  /**
   * @return None: Connection has been closed before next message
   */
  def receiveMessage(): Option[ByteBuffer] = {
    val b = receiveBytes(4)
    if (b.limit() == 0)  // End?
      None
    else
      Some(receiveBytes(b.getInt))
  }

  private def receiveBytes(n: Int): ByteBuffer = {
    val buffer = ByteBuffer.allocate(n)
    var offset = 0
    var end = false
    while (offset < n && !end) {
      val ret = blocking { in.read(buffer.array(), offset, n - offset) }
      if (ret > 0) {
        offset += ret
      } else {
        if (offset > 0) sys.error("Connection has been closed unexpectedly")
        end = true
      }
    }
    buffer.limit(offset)
    buffer.rewind()
    buffer
  }

  def sendMessage(data: Array[Byte], length: Int): Unit = {
    // Send as one TCP packet, with one write
    val b = ByteBuffer.allocate(4 + length)
    b.putInt(length)
    b.put(data, 0, length)
    blocking {
      out.write(b.array)
    }
  }
}

object TcpConnection{
  private val logger = Logger(getClass)
}
