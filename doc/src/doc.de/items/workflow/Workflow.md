# Workflow

Beispiele:
```json
{
  "instructions": [
    {
      "TYPE": "Execute.Anonymous",
      "job": {
        "agentId": "AGENT",
        "executable": {
          "TYPE": "PathExecutable",
          "file": "script.sh"
        }
      }
    }
  ]
}
```

```json
{
  "instructions": [
    {
      "label": "TEST-LABEL",
      "TYPE": "Execute.Named",
      "jobName": "MY-JOB"
    }
  ],
  "jobs": {
    "MY-JOB": {
      "agentId": "AGENT",
      "executable": {
        "TYPE": "ScriptExecutable",
        "script": "#!/usr/bin/env bash\n set -euo pipefail\necho HELLO\n"
      }
    }
  }
}
```

**Parameter**

* `instructions`

  Eine Liste von [Anweisungen](Instruction.md).

* `jobs`

  Eine Map von [WorkflowJob](WorkflowJob.md),
  die im Workflow mit `Execute.Named` aufrufbar sind.
