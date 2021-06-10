# Workflow

Beispiele in JSON:
```json
{
  "instructions": [
    {
      "TYPE": "Execute.Anonymous",
      "job": {
        "agentPath": "AGENT",
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
  "path": "MY-WORKFLOW",
  "versionId": "0.1",
  "orderRequirements": {
    "parameters": {
      "myString": {
        "type": "String",
        "default": "MY-DEFAULT"
      },
      "myNumber": {
        "type": "Number"
      }
    }
  },
  "jobResourcePaths": [
    "JOB-RESOURCE"
  ],
  "instructions": [
    {
      "label": "TEST-LABEL",
      "TYPE": "Execute.Named",
      "jobName": "MY-JOB"
    }
  ],
  "jobs": {
    "MY-JOB": {
      "agentPath": "AGENT",
      "executable": {
        "TYPE": "ScriptExecutable",
        "script": "#!/usr/bin/env bash\n set -euo pipefail\necho HELLO\n"
      }
    }
  }
}
```

**Felder**

* orderRequirements (optional)
  * parameters

    Eine Abbildung mit den Namen der verlangten Auftragsparameter auf der linken
    und der Beschreibung auf der rechten Seite.
    Die Parameterdeklaration hat die Felder
    * type

      "String" | "Boolean" | "Number"
    * default (optional)

      Der voreingestellte Wert

* `jobResourcePaths`
  JobResourcePaths, die für die Jobs im Workflow verwendet werden sollen.
  Sie gelten zusätzlich zu den an den einzelnen Jobs bestimmten JobResourcen.

* `instructions`

  Eine Liste von [Anweisungen](Instruction.md).

* `jobs`

  Eine Map von [WorkflowJob](WorkflowJob.md),
  die im Workflow mit `Execute.Named` aufrufbar sind.

## Deklarierte Auftragsparameter

Ein Workflow kann von seinen Aufträgen bestimmte Parameter (Feld `arguments` im Auftrag) verlangen.
Für hinzufügende Aufträge gilt
* Ein Auftragsparameter ist optional, wenn ein Default-Wert bestimmt ist.
* Der Auftrag muss für alle Parameter ohne Default einen Wert angeben.
* Der Typ des Werts muss mit dem deklarierten Typ übereinstimmen.
* Ein Auftrag mit nicht deklarierten Parametern wird ablehnt.

Beispiel in JSON den beiden Parametern
* `myString: String = "DEFAULT"`
* `myNumber: Number` (nicht optional)

```json
{
  "path": "PARAMETERIZED-WORKFLOW",
  "orderRequirements": {
    "parameters": {
      "myString": {
        "type": "String",
        "default": "DEFAULT"
      },
      "myNumber": {
        "type": "Number"
      }
    }
  },
  "instructions": [
    {
      "TYPE": "Execute.Anonymous",
      "job": {
        "agentPath": "AGENT",
        "executable": {
          "TYPE": "ScriptExecutable",
          "script": "#!/usr/bin/env bash\nset -euo pipefail\necho \"STRING=$STRING\"\necho \"NUMBER=$NUMBER\"\n",
          "env": {
            "STRING": "$myString",
            "NUMBER": "$myNumber"
          }
        },
        "parallelism": 1
      }
    }
  ]
}
```
