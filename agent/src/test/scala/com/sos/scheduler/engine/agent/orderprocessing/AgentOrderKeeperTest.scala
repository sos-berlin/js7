//package com.sos.scheduler.engine.agent.orderprocessing
//
//import AgentOrderKeeperTest._
//import akka.actor.{ActorSystem, Props}
//import akka.util.Timeout
//import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
//import com.sos.scheduler.engine.agent.data.commands.AddJobNet
//import com.sos.scheduler.engine.common.auth.UserId
//import com.sos.scheduler.engine.common.event.EventIdGenerator
//import com.sos.scheduler.engine.common.scalautil.FileUtils.deleteDirectoryRecursively
//import com.sos.scheduler.engine.common.scalautil.{FileUtils, Logger}
//import com.sos.scheduler.engine.common.time.timer.TimerService
//import com.sos.scheduler.engine.shared.event.SnapshotKeyedEventBus
//import org.scalatest.{BeforeAndAfterAll, FreeSpec}
//import scala.collection.{immutable, mutable}
//import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
//import com.sos.scheduler.engine.common.time.ScalaTime._
//import com.sos.scheduler.engine.data.engine2.agent.AgentPath
//import com.sos.scheduler.engine.data.engine2.order.{JobChainPath, JobNet, JobPath, NodeId}
//import java.nio.file.Files
//import scala.concurrent.duration.DurationInt
//
///**
//  * @author Joacim Zschimmer
//  */
//final class AgentOrderKeeperTest extends FreeSpec with BeforeAndAfterAll {
//
//  private val timerService = TimerService()
//  private lazy val actorSystem = ActorSystem("AgentOrderKeeperTest")
//  private lazy val stateDirectory = Files.createTempDirectory("AgentOrderKeeperTest-")
//
//  private lazy val orderKeeper = actorSystem actorOf Props {
//    new AgentOrderKeeper(
//      UserId("TEST"),
//      stateDirectory = stateDirectory,
//      askTimeout = Timeout(30.seconds),
//      new SnapshotKeyedEventBus,
//      new EventIdGenerator,
//      timerService)}
//
//  override protected def afterAll() = {
//    actorSystem.terminate() await 99.s
//    timerService.close()
//    deleteDirectoryRecursively(stateDirectory)
//    super.afterAll()
//  }
//
//  "start" in {
//    orderKeeper ! AgentOrderKeeper.Input.Start(Nil)
//    orderKeeper ! AddJobNet(TestJobnet)
//  }
//
//  "test" in {
//
//  }
//}
//
//object AgentOrderKeeperTest {
//  private val logger = Logger(getClass)
//  private val TestJobnetPath = JobChainPath("/TEST-JOBNET")
//  private val TestJobnet = JobNet(TestJobnetPath, NodeId("100"), Map(
//     NodeId("100") → JobNet.JobNode(NodeId("100"), AgentPath("/TEST-AGENT"), JobPath("/TEST-JOBS"), NodeId("END"), NodeId("END")),
//     NodeId("END") → JobNet.EndNode(NodeId("ERROR"))))
//}
