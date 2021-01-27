# v2.0.0-alpha.20210127

## Deklarierte Auftragsparameter

Ein Workflow kann von seinen Aufträgen bestimmte Parameter (im Feld `arguments` des Auftrags) verlangen.
Für hinzufügende Aufträge gilt:
* Ein Auftragsparameter ist optional, wenn ein Default-Wert bestimmt ist.
* Der Auftrag muss für alle Parameter ohne Default einen Wert angeben.
* Der Typ des Werts muss mit dem deklarierten Typ übereinstimmen.
* Ein Auftrag mit anderen Parametern wird ablehnt.

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
        "agentId": "AGENT",
        "executable": {
          "TYPE": "ScriptExecutable",
          "script": "#!/usr/bin/env bash\nset -euo pipefail\necho \"STRING=$STRING\"\necho \"NUMBER=$NUMBER\"\n",
          "env": {
            "STRING": "$myString",
            "NUMBER": "$myNumber"
          }
        },
        "taskLimit": 1
      }
    }
  ]
}
```
