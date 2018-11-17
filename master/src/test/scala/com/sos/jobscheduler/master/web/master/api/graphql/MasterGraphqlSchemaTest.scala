package com.sos.jobscheduler.master.web.master.api.graphql

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, VersionId}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ForkJoin}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.web.master.api.graphql.MasterGraphqlSchemaTest._
import io.circe.Json
import org.scalatest.FreeSpec
import sangria.ast.Document
import sangria.execution.Executor
import sangria.macros._
import sangria.marshalling.circe._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class MasterGraphqlSchemaTest extends FreeSpec {

  "order" in {
    testGraphql(
      graphql"""{
        order(id: "11") {
          id
          workflowPath
        }
      }""",
      json"""{
        "data": {
          "order": {
            "id": "11",
            "workflowPath": "/A-WORKFLOW"
          }
        }
      }""")
  }

  "order with variable" in {
    testGraphql(
      graphql"""
        query Order($$id: OrderId!) {
          order(id: $$id) {
            id
            workflowPath
          }
        }""",
      """{
        "id": "11"
      }""".parseJson,
      json"""{
        "data": {
          "order": {
            "id": "11",
            "workflowPath": "/A-WORKFLOW"
          }
        }
      }""")
  }

  private val completeOrderGraphql = graphql"""{
      orders {
        id
        workflowPosition {
          workflowId {
            path
            versionId
          }
          position
          instruction {
            TYPE
            ... on Execute_Named {
              name
            }
            ... on Execute_Anonymous {
              job {
                agentPath
                executablePath
                taskLimit
              }
            }
          }
        }
        attachedTo {
          TYPE
          agentId {
            path
            versionId
          }
        }
        state {
          TYPE
          ... on FreshOrderState {
            scheduledAt
          }
          ... on ProcessedOrderState {
            outcome {
              ...Outcome
            }
          }
          ... on StoppedOrderState {
            outcome {
              ...Outcome
            }
          }
          ... on ForkOrderState {
            childOrderIds
          }
          ... on AwaitingOrderState {
            offeredOrderId
          }
          ... on BrokenOrderState {
            message
          }
        }
        variables
      }
    }

    fragment Outcome on Outcome {
      TYPE
      ... on SucceededOutcome {
        returnCode
      }
      ... on FailedOutcome {
        returnCode
      }
      ... on DisruptedOutcome {
        reason {
          TYPE
          message
        }
      }
    }"""

  "orders" in {
    testGraphql(completeOrderGraphql,
      json"""{
        "data": {
          "orders": [
            {
              "id": "11",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 0 ],
                "instruction": {
                  "TYPE": "Execute.Named",
                  "name": "JOB"
                }
              },
              "state": {
                "TYPE": "Fresh",
                "scheduledAt": 1523877753000
              }
            }, {
              "id": "12",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 1 ],
                "instruction": {
                  "TYPE": "ForkJoin"
                }
              },
              "state": {
                "TYPE": "Ready"
              }
            }, {
              "id": "13",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 1, "BRANCH", 0 ],
                "instruction": {
                "TYPE": "Execute.Anonymous",
                  "job": {
                    "agentPath": "/AGENT",
                    "executablePath": "/TEST.sh",
                    "taskLimit": 1
                  }
                }
              },
              "attachedTo": {
                "TYPE": "Agent",
                "agentId": {
                  "path": "/AGENT",
                  "versionId": "2"
                }
              },
              "state": {
                "TYPE": "InProcess"
              },
              "variables": {
                "KEY": "VALUE",
                "X": "XX"
              }
            }, {
              "id": "14",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Processed",
                "outcome": {
                  "TYPE": "Succeeded",
                  "returnCode": 7
                }
              }
            }, {
              "id": "15",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Processed",
                "outcome": {
                  "TYPE": "Failed",
                  "returnCode": 8
                }
              }
            }, {
              "id": "16",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Stopped",
                "outcome": {
                  "TYPE": "Disrupted",
                  "reason": {
                    "TYPE": "Other",
                    "message": "MESSAGE"
                  }
                }
              }
            }, {
              "id": "17",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Processed",
                "outcome": {
                  "TYPE": "Disrupted",
                  "reason": {
                    "TYPE": "JobSchedulerRestarted"
                  }
                }
              }
            }, {
              "id": "18",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Forked",
                "childOrderIds": [ "A/1", "B/1" ]
              }
            }, {
              "id": "19",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Offered"
              }
            }, {
              "id": "20",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Awaiting",
                "offeredOrderId": "OFFERED"
              }
            }, {
              "id": "21",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Finished"
              }
            }, {
              "id": "22",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 2 ],
                "instruction": {
                  "TYPE": "ImplicitEnd"
                }
              },
              "state": {
                "TYPE": "Broken",
                "message": "PROBLEM"
              }
            }
          ]
        }
      }""")
  }

  if (sys.props contains "test.speed") "Speed test" - {
    val n = 100000
    def run: Json = Executor.execute(MasterGraphqlSchema.schema, completeOrderGraphql, bigContext(n)) await 99.s

    "Internal Json" in {
      for (_ ← 1 to 3) {
        val stopwatch = new Stopwatch
        run
        info(stopwatch.itemsPerSecondString(n, "orders(Json)"))
      }
    }

    "String JSON" in {
      val stopwatch = new Stopwatch
      run
      info(stopwatch.itemsPerSecondString(n, "orders(JSON)"))
    }
  }

  private def testGraphql(query: Document, expectedResult: Json): Unit =
    testGraphql(query, Json.obj(), expectedResult)

  private def testGraphql(query: Document, variables: Json, expectedResult: Json): Unit =
    assert(CompactPrinter.pretty(executeGraphql(query, variables) await 99.s).parseJson == expectedResult)

  private def executeGraphql(query: Document, variables: Json): Future[Json] =
    Executor.execute(MasterGraphqlSchema.schema, query, TestContext, variables = variables)
}

object MasterGraphqlSchemaTest
{
  private trait TestContext extends QueryContext {
    def executionContext = ExecutionContext.global

    protected val idToOrder = Vector[Order[Order.State]](
        Order(OrderId("11"), (WorkflowPath("/A-WORKFLOW") % "1") /: Position(0), Order.Fresh(Some(Timestamp.parse("2018-04-16T11:22:33Z")))),
        Order(OrderId("12"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(1), Order.Ready),
        Order(OrderId("13"), (WorkflowPath("/A-WORKFLOW") % "1") /: Position(1, "BRANCH", 0), Order.InProcess,
          Some(Order.AttachedTo.Agent(AgentPath("/AGENT") % "2")),
          Some(OrderId("PARENT")),
          Payload(Map("KEY" → "VALUE", "X" → "XX"))),
        Order(OrderId("14"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Processed(Outcome.Succeeded(ReturnCode(7)))),
        Order(OrderId("15"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Processed(Outcome.Failed(ReturnCode(8)))),
        Order(OrderId("16"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Stopped(Outcome.Disrupted(Problem("MESSAGE")))),
        Order(OrderId("17"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Processed(Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted))),
        Order(OrderId("18"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Forked(Order.Forked.Child("A", OrderId("A/1")) :: Order.Forked.Child("B", OrderId("B/1")) :: Nil)),
        Order(OrderId("19"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Offered(Timestamp.parse("2018-04-16T11:22:33Z"))),
        Order(OrderId("20"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Awaiting(OrderId("OFFERED"))),
        Order(OrderId("21"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Finished),
        Order(OrderId("22"), (WorkflowPath("/B-WORKFLOW") % "1") /: Position(2), Order.Broken(Problem("PROBLEM")))
      ).toKeyedMap(_.id)

    def order(orderId: OrderId) = Future.successful(idToOrder.get(orderId))

    def orders(filter: QueryContext.OrderFilter) = Future.successful(idToOrder.values.toVector take filter.limit)

    def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
      Future.successful(id match {
        case FileBasedId(_: WorkflowPath, VersionId("1")) ⇒
          Valid(Workflow(
            WorkflowPath.NoId,
            Vector(
              Execute(WorkflowJob.Name("JOB")),
              ForkJoin(Vector(
                ForkJoin.Branch("BRANCH", Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/TEST.sh")))))
              ))),
            Map(WorkflowJob.Name("JOB") → WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/TEST.sh")))).asInstanceOf[A])
        case _ ⇒ Problem(s"No such ''$id'")
    })
  }
  private object TestContext extends TestContext

  private def bigContext(n: Int): QueryContext = new TestContext {
    private val order = Order(OrderId("?"), (WorkflowPath("/A-WORKFLOW") % "1") /: Position(0), Order.Fresh(Some(Timestamp.parse("2018-04-16T11:22:33Z"))))
    override val idToOrder = (1 to n).map(i ⇒ order.copy(id = OrderId(s"$i"))) toKeyedMap (_.id)
  }
}
