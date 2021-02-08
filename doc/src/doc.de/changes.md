# v2.0.0-alpha.20210131

* Fix: Die Lock-Anweisung erlaubt jetzt Jobs verschiedener Agenten
* Fix: Ein leerer Workflow blockiert nicht mehr den Auftrag. Der Auftrag wird nun nach dem Start sofort beendet.
* Fix: defaultArguments von Job und Execute-Anweisung werden jetzt korrekt berücksichtigt (Execute hat Vorrang vor dem Job).
* Experimentell: InternalExecutable zum Ausführen interner, als Klasse definierter Jobs; zunächst für Scala

## InternalExecutable
`InternalExecutable` erlaubt es (zunächst experimentell) einen Job als JVM-Klasse mit Scala-Schnittstelle, später auch mit Java-Schnittstelle zu definieren. So ein Job startet also keinen Prozess, sondern läuft in der JVM des Agenten.

Beispiel für einen Aufruf:
```json
"executable": {
  "TYPE": "InternalExecutable",
  "className": "js7.tests.jobs.EmptyJob"
}
```
Der in JS7 bereits vordefinierte interne Job `js7.tests.jobs.EmptyJob` tut nichts.

----------------------------------------------------------------------------------------------------
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
