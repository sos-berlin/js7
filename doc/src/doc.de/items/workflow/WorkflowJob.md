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
  "sigkillDelay": 15,
  "timeout": 3600
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

  Default: Die [Einstellung](../../conf.md) `js7.job.execution.sigkill-delay`
  in der Datei `agent.conf`.

* `timeout`

### Timeout
Wenn ein Job mit einem Timeout versehen ist und nach Verstreichen der Frist der Job noch läuft, dann bricht JS7 ihn ab
und geht dabei wie bei `CancelOrder` vor.
`sigkillDelay` wird beachtet.
Der Auftragsschritt scheitert, was mit try/catch abgefangen werden kann.

Outcome in JSON:
```json
{
  "TYPE": "TimedOut",
  "outcome": {
    "TYPE": "Failed",
    "namedValues": {
      "returnCode": 143
    }
  }
}
```

Die Java-Schnittstelle kennt das neue `JOutcome.TimedOut(Outcome)`.
