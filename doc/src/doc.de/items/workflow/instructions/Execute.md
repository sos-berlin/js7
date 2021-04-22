# Execute

Die Execute-Anweisung führt einen Job mit dem Auftrag aus.

## Execute.Named

Führt einem benannten [WorkflowJob](../WorkflowJob.md) aus,
der am Ende des Workflows definiert sein muss.

```json
{
  "TYPE": "Execute.Named",
  "jobName": "MY-JOB",
  "defaultArguments": {
    "ARG": "VALUE"
  }
}
```

**Felder**

* `jobName`

  der Name des Jobs, wie er im Workflow definiert ist.

* `defaultArguments`

  Parameter für den Job,
  soweit sie nicht schon vom Auftrag, vom Job oder von weiteren vorrangigen Quellen
  gesetzt worden sind.


## Execute.Anonymous

Führt einen unbenannten [WorkflowJob](../WorkflowJob.md) aus,
der direkt in dieser Anweisung definiert ist.

```json
{
  "TYPE": "Execute.Anonymous",
  "job": {
    "agentPath": "AGENT",
    "executable": {
      "TYPE": "ScriptExecutable",
      "script": "#!/bin/sh\n..."
    }
  }
}
```

**Felder**

* `job`

  Der [WorkflowJob](../WorkflowJob.md).
