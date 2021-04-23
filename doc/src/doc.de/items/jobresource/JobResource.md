# JobResource

Einem Job lassen sich JobResourcen zugeordnen, die Umgebungsvariablen für den Job bestimmen (wenn es nicht ein JVM-Job ist).

Beispiel in JSON:

````json
{
  "path": "MY-JOB-RESOURCE",
  "TYPE": "JobResource",
  "env": {
    "MYENV": "\"VALUE\"",
    "MYPATH": "\"/bin:\" ++ env(\"PATH\")"
  }
}
````

Funktionen und Variablen der Ausdruckssprache speziell für JobResource:

### Funktion env(name)
liefert den Wert einer Umgebungsvariablen des Agenten-Prozesses.
Die Funktion scheitert, wenn die Umgebungsvariable unbekannt ist.

### Funktion now(format=FORMAT, timezone=TIMEZONE)
(Wie schon für FileWatch.)
Liefert den aktuellen Zeitpunkt (also bei Entstehung des Events ExternalOrderArised),
formatiert mit DateTimeFormatter und übersetzt in die angegebene Zeitzone (UTC, wenn nicht angegeben).

Der Ausdruck im JSON-Beispiel
`now(format='yyyy-MM-dd', timezone='Antarctica/Troll')`
liefert zum Beispiel `"2021-04-23"`.

Beispiele
- `now(format='yyyy-MM-dd', timezone='Antarctica/Troll')`
- `now(format='yyyy-MM-dd')`  (UTC-Zeitzone)

### Variablen
- `$epochSecond`: Zahl der Sekunden seit dem 1. Januar 1970 UTC.
- `$epochMilli`: Zahl der Millisekunden seit dem 1. Januar 1970 UTC.
- `$js7ControllerId`
- `$js7WorkflowPath`
- `$js7WorkflowPosition`: WorkflowId (Path + VersionId) und Position im Workflow als String
- `$js7OrderId`
- `$js7JobName`

JobResourcen können so in einem Job referenziert werden:
```json
{
  "path": "WORKFLOW-ENV",
  "versionId": "1.0",
  "instructions": [
    {
      "TYPE": "Execute.Anonymous",
      "job": {
        "agentPath": "AGENT",
        "jobResourcePaths": [
          "MY-JOB-RESOURCE"
        ],
        "executable": {
          "TYPE": "ScriptExecutable",
          "script": "#!/usr/bin/env bash\nset -euo pipefail\necho MYENV=/$MYENV/\n"
        }
      }
    }
  ]
}
```
Wenn mehrere JobResourcen angegeben sind, die gleiche Namen für Umgebungsvariablen enthalten,
dann gelten die _zuerst_ angegeben (links hat Vorrang).
Dabei ist auch unter Windows ist die Groß/Kleinschreibung relevant.
Für Jobs, die unter Windows laufen,
sollte für Umgebungsvariablen eine einheitliche Großschreibung gewählt werden.


### Abruf am Workflow

JobResourcen, die jedem Job eines Workflows mitgegeben werden sollen,
können am Workflow mit dem Parameter `jobResourcePaths` bestimmt werden.
Sie werden der Liste der JobResourcen am Job angehängt.
