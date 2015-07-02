package com.sos.scheduler.engine.common.tcp

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.TcpConnection._
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousCloseException, ServerSocketChannel, SocketChannel}

final class TcpConnection(val channel: SocketChannel) extends AutoCloseable with MessageConnection {

  val ownAddress: InetSocketAddress = channel.getLocalAddress.asInstanceOf[InetSocketAddress]
  val peerAddress: InetSocketAddress = channel.getRemoteAddress.asInstanceOf[InetSocketAddress]

  def close(): Unit = {
    logger.debug(s"close $peerAddress")
    channel.close()
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
    do channel.read(buffer) while (buffer.position > 0 && buffer.position < buffer.limit)
    assert(buffer.position == 0 || buffer.position == buffer.limit)
  }

  def sendMessage(data: ByteString): Unit = sendMessage(data.asByteBuffers, data.size)

  def sendMessage(data: Array[Byte], length: Int): Unit = sendMessage(List(ByteBuffer.wrap(data, 0, length)), length)

  private def sendMessage(byteBuffers: Iterable[ByteBuffer], size: Int): Unit = {
    val lengthBuffer = ByteBuffer.allocate(4)
    lengthBuffer.putInt(size)
    lengthBuffer.flip()
    // Send as one TCP packet, with one write
    channel.write(Array(lengthBuffer) ++ byteBuffers)
  }

  override def toString = s"TcpConnection($peerAddress)"

  def ownPort: Int = ownAddress.getPort
  def isConnected = channel.isConnected
}

object TcpConnection{
  private val logger = Logger(getClass)

  def connect(peerAddress: InetSocketAddress): TcpConnection = {
    logger.debug(s"Connecting with $peerAddress ...")
    val channel = SocketChannel.open()
    channel.connect(peerAddress)
    logger.debug(s"Connected own ${channel.getLocalAddress} with remote $peerAddress")
    assert(channel.isBlocking)
    new TcpConnection(channel)
  }

  final class Listener(address: InetSocketAddress) extends AutoCloseable {
    private val listener = ServerSocketChannel.open().bind(address)
    val boundAddress = listener.getLocalAddress.asInstanceOf[InetSocketAddress]

    def accept() = new TcpConnection(listener.accept())

    def close() = listener.close()
  }

  object Listener {
    def forLocalHostPort(port: Int = 0) = new Listener(new InetSocketAddress("127.0.0.1", port))
  }
}
