# WorkflowJob

Beispiel in JSON:
```json
{
  "agentPath": "AGENT",
  "executable": {
    "TYPE": "ShellScriptExecutable",
    "script": "echo \"hello world1\"\n"
  },
  "defaultArguments": {
    "NAME": "'VALUE'",
    "NUMBER": "7"
  },
  "parallelism": 1,
  "sigkillDelay": 15
}
```
**Felder**
* `agentPath`
* `executable`

  Ein [Executable](Executable.md)

* `defaultArguments`

* `parallelism`

  Wie viele Prozesse dieses Jobs gleichzeitig ausgeführt werden können.

  Default: `parallelism=1`

* `sigKillDelay`

  Nur Unix.
  Wenn nach dem Versuch, einen Prozess abzubrechen,
  der Prozess nach der angegebenen Zeitspanne noch läuft,
  dann sendet JS7 dem Prozess ein `SIGKILL`-Signal.

  Default: Die [Einstellung](../../settings.md) `js7.job.execution.sigkill-delay`
  in der Datei `agent.conf`.
