package com.sos.jobscheduler.master.web.master.api.graphql

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, VersionId}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, Fork}
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
final class MasterGraphqlSchemaTest extends FreeSpec
{
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
      json"""{
        "id": "11"
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

  private val completeOrderGraphql = graphql"""
    {
      orders {
        id
        workflowPath
        workflowPosition {
          workflowId {
            path
            versionId
          }
          position
          instruction {
            TYPE
            ... on Execute_Named {
              jobName
            }
            ... on Execute_Anonymous {
              job {
                agentRefPath
                executablePath
                taskLimit
              }
            }
          }
        }
        attachedState {
          TYPE
          agentRefPath
        }
        state {
          TYPE
        }
        scheduledFor
        outcome {
          ...Outcome
        }
        childOrderIds
        offeredOrderId
        problem {
          message
        }
        variables
      }
    }

    fragment Outcome on Outcome {
      TYPE
      returnCode
      reason {
        TYPE
        problem {
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
              "workflowPath": "/A-WORKFLOW",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 0 ],
                "instruction": {
                  "TYPE": "Execute.Named",
                  "jobName": "JOB"
                }
              },
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Fresh"
              },
              "scheduledFor": 1523877753000
            }, {
              "id": "12",
              "workflowPath": "/A-WORKFLOW",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 0 ],
                "instruction": {
                  "TYPE": "Execute.Named",
                  "jobName": "JOB"
                }
              },
              "attachedState": {
                "TYPE": "Attaching",
                "agentRefPath": "/AGENT"
              },
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Fresh"
              },
              "scheduledFor": 1523877753000
            }, {
              "id": "13",
              "workflowPath": "/A-WORKFLOW",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 0 ],
                "instruction": {
                  "TYPE": "Execute.Named",
                  "jobName": "JOB"
                }
              },
              "attachedState": {
                "TYPE": "Attached",
                "agentRefPath": "/AGENT"
              },
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Fresh"
              },
              "scheduledFor": 1523877753000
            }, {
              "id": "14",
              "workflowPath": "/B-WORKFLOW",
              "workflowPosition": {
                "workflowId": {
                  "path": "/B-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 1 ],
                "instruction": {
                  "TYPE": "Fork"
                }
              },
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Ready"
              },
              "attachedState": {
                "TYPE": "Attached",
                "agentRefPath": "/AGENT"
              }
            }, {
              "id": "15",
              "workflowPath": "/A-WORKFLOW",
              "workflowPosition": {
                "workflowId": {
                  "path": "/A-WORKFLOW",
                  "versionId": "1"
                },
                "position": [ 1, "fork+BRANCH", 0 ],
                "instruction": {
                "TYPE": "Execute.Anonymous",
                  "job": {
                    "agentRefPath": "/AGENT",
                    "executablePath": "/TEST.sh",
                    "taskLimit": 1
                  }
                }
              },
              "attachedState": {
                "TYPE": "Attached",
                "agentRefPath": "/AGENT"
              },
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Processing"
              },
              "variables": {
                "KEY": "VALUE",
                "X": "XX"
              }
            }, {
              "id": "16",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 7
              },
              "state": {
                "TYPE": "Processed"
              }
            }, {
              "id": "17",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Failed",
                "returnCode": 8
              },
              "state": {
                "TYPE": "Processed"
              }
            }, {
              "id": "18",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Disrupted",
                "reason": {
                  "TYPE": "Other",
                  "problem": {
                    "message": "MESSAGE"
                  }
                }
              },
              "state": {
                "TYPE": "Stopped"
              }
            }, {
              "id": "19",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Disrupted",
                "reason": {
                  "TYPE": "JobSchedulerRestarted"
                }
              },
              "state": {
                "TYPE": "Processed"
              }
            }, {
              "id": "20",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Forked"
              },
              "childOrderIds": [ "A/1", "B/1" ]
            }, {
              "id": "21",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Offering"
              }
            }, {
              "id": "22",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Awaiting"
              },
              "offeredOrderId": "OFFERED"
            }, {
              "id": "23",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Finished"
              }
            }, {
              "id": "24",
              "workflowPath": "/B-WORKFLOW",
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
              "outcome": {
                "TYPE": "Succeeded",
                "returnCode": 0
              },
              "state": {
                "TYPE": "Broken"
              },
              "problem": {
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
      for (_ <- 1 to 3) {
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
    assert(CompactPrinter.pretty(executeGraphql(query, variables) await 99.s).parseJsonOrThrow == expectedResult)

  private def executeGraphql(query: Document, variables: Json): Future[Json] =
    Executor.execute(MasterGraphqlSchema.schema, query, TestContext, variables = variables)
}

object MasterGraphqlSchemaTest
{
  private def bigContext(n: Int): QueryContext = new TestContext {
    private val order = Order(OrderId("?"), (WorkflowPath("/A-WORKFLOW") ~ "1") /: Position(0), Order.Fresh(Some(Timestamp.parse("2018-04-16T11:22:33Z"))))
    override val idToOrder = (1 to n).map(i => order.copy(id = OrderId(s"$i"))) toKeyedMap (_.id)
  }
  private object TestContext extends TestContext

  private trait TestContext extends QueryContext {
    def executionContext = ExecutionContext.global
    private val agentRefPath = AgentRefPath("/AGENT")
    private val fresh = Order.Fresh(Some(Timestamp.parse("2018-04-16T11:22:33Z")))
    private val attached = Some(Order.Attached(agentRefPath))

    protected val idToOrder = Vector[Order[Order.State]](
        Order(OrderId("11"), (WorkflowPath("/A-WORKFLOW") ~ "1") /: Position(0), fresh),
        Order(OrderId("12"), (WorkflowPath("/A-WORKFLOW") ~ "1") /: Position(0), fresh, attachedState = Some(Order.Attaching(agentRefPath))),
        Order(OrderId("13"), (WorkflowPath("/A-WORKFLOW") ~ "1") /: Position(0), fresh, attachedState = attached),
        Order(OrderId("14"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(1), Order.Ready, attachedState = attached),
        Order(OrderId("15"), (WorkflowPath("/A-WORKFLOW") ~ "1") /: (Position(1) / "fork+BRANCH" % 0), Order.Processing,
          attachedState = Some(Order.Attached(AgentRefPath("/AGENT"))),
          parent = Some(OrderId("PARENT")),
          payload = Payload(Map("KEY" -> "VALUE", "X" -> "XX"))),
        Order(OrderId("16"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Processed, Outcome.Succeeded(ReturnCode(7))),
        Order(OrderId("17"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Processed, Outcome.Failed(ReturnCode(8))),
        Order(OrderId("18"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Stopped  , Outcome.Disrupted(Problem("MESSAGE"))),
        Order(OrderId("19"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Processed, Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted)),
        Order(OrderId("20"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Forked(Order.Forked.Child("A", OrderId("A/1")) :: Order.Forked.Child("B", OrderId("B/1")) :: Nil)),
        Order(OrderId("21"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Offering(Timestamp.parse("2018-04-16T11:22:33Z"))),
        Order(OrderId("22"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Awaiting(OrderId("OFFERED"))),
        Order(OrderId("23"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Finished),
        Order(OrderId("24"), (WorkflowPath("/B-WORKFLOW") ~ "1") /: Position(2), Order.Broken(Problem("PROBLEM")))
      ).toKeyedMap(_.id)

    def order(orderId: OrderId) = Future.successful(idToOrder.get(orderId))

    def orders(filter: QueryContext.OrderFilter) = Future.successful(idToOrder.values.toVector take filter.limit)

    def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
      Future.successful(id match {
        case FileBasedId(_: WorkflowPath, VersionId("1")) =>
          Valid(Workflow(
            WorkflowPath.NoId,
            Vector(
              Execute(WorkflowJob.Name("JOB")),
              Fork(Vector(
                Fork.Branch("BRANCH", Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/TEST.sh")))))
              ))),
            Map(WorkflowJob.Name("JOB") -> WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/TEST.sh")))).asInstanceOf[A])
        case _ => Problem(s"No such ''$id'")
    })
  }
}
