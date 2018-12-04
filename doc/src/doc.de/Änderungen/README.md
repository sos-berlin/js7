# Änderungen

## 2018-12-04

### Neues Kommando CancelOrder

Zur Stornierung von Aufträgen.
Das Kommando antwortet mit ```{ "TYPE": "Accepted" }```, wenn die OrderId bekannt ist, andernfalls mit einem Fehler.
Das Kommando vermerkt den Stornierungswusch. 
Die Stornierung selbst ist asynchron. 
Der Master verfolgt dazu den Auftrag bis auf den Agenten, wo er storniert wird. Anschließend wird er zurückgeholt und gelöscht.

Storniert werden können Aufträge
- die kein Kindauftrag sind (Fork)
- die noch nicht gestartet sind (erster Job noch nicht gestartet und keine Fork-Anweisung am Anfang des Workflow)
- optional mit dem Parameter ````mode``` 
  - ```"mode": { "TYPE": "NotStarted" }``` nur Aufträge, die noch nicht gestartet sind (voreingestellt) 
  - ```"mode": { "TYPE": "FreshOrStarted" }``` auch Aufträge, die gestartet sind und 

CancelOrder ist wirkungslos bei Aufträgen, die das Ende des Workflows erreicht haben.
Diese Aufträge beenden sich normal.

Ein Auftrag wird nur storniert, wenn er im Zustand Ready ist, zum Beispiel nach einem Fork oder einer Job-Ausführung.
In anderen Fällen wird der Stornierungswunsch vorgemerkt.                                             

**JSON**
```
{
  "TYPE": "CancelOrder",
  "orderId": "MY-ORDER-ID",
  "mode": {
    "TYPE": "FreshOrStarted"
  }
}
```

Der Master nimmt Kommandos über den Webservice ```POST /master/api/command``` entgegen.

### Neues Kommando Batch

Erlaubt die Übergabe von mehreren Kommandos, die der Master unabhängig voneinander und ungeordnet ausführt.

Beispiel für zwei CancelOrder:
```
{
  "TYPE": "Batch",
  "commands": [
    {
      "TYPE": "CancelOrder",
      "orderId": "UNKNOWN-ORDER"
    }, {
      "TYPE": "CancelOrder",
      "orderId": "ANOTHER-ORDER"
    }
  ]
}
```

Die Antwort enthält das Array ```responses```.
Das Array enthält für jedes Kommando ein Objekt mit folgendem Inhalt.
- Wenn ein Problem auftretreten ist
  - ```"TYPE": "Problem"```
  - ```"code": "(fehlercode)"```, optional (einziger Fehlercode bislang: ```"UnknownOrder"```)
  - ```"message": "(fehlertext)"```
- Wenn kein Problem aufgetreten ist
  - ```"TYPE": "Accepted"```, oder abhängig vom Kommando ein anderer Typ, aber nicht "Problem".  
  
JSON auf das obige Batch-Kommando, wenn der Auftrag UNKNOWN-ORDER nicht bekannt ist.
 
```
{
  "TYPE": "BatchResponse",
  "responses": [
    {
      "TYPE": "Problem",
      "code": "UnknownOrder",
      "insertions": [
        "UNKNOWN"
      ],
      "message": "Unknown OrderId 'UNKNOWN-ORDER'"
    }, {
      "TYPE": "Accepted"
    }
  ]
}
```

Wenn der Master schob bei Annahme des Kommandos erkennt, dass der Auftrag nicht storniert werden kann,
dann antwortet er mit einer Fehlermeldung (```"TYPE": "Problem"```). 


## 2018-10-20

### Geänderte Workflow-Syntax 

Ein Workflow wird jetzt definiert mit ```define workflow { ... }```.
Zum Beispiel:

```
define workflow {
  job A;
  ...
}
```

Jobs werden nicht mehr am Agenten, sondern im Workflow definiert:

```
define workflow {
  job A, arguments={"hello": "Hello!"};
  job B;
  
  define job A {
    execute executable="/test-A", agent="agent-1";
  }
  
  define job B {
    execute executable="/test-B", agent="agent-2", taskLimit=30, arguments={"key": "value", "hello": "გამარჯობა!"};
  }
}
```

### Neue Workflow-Anweisungen

- ```job``` gibt den Namen eines Jobs an, der im Workflow mit ```define job``` definiert ist.
- ```define job``` definiert einen Job mit genau einer ```execute```-Anweisung.
- ```execute``` führt ein Programm _(executable)_ des Agenten aus. Die Anweisung hat die Parameter
  - executable=_ExecutablePath_ – Der Pfad des Programms muss mit einem Schrägstrich beginnen und darf weder die Verzeichnisse ".." noch "." enthalten.
  - agent=_AgentPath_
  - arguments=_JsonObject_ – Parameter für das Programm, notiert wie ein JSON-Objekt mit String-Werten. 
    Der Prozess erhält die Parameter als großgeschriebene Umgebungsvariablen mit dem Präfix "SCHEDULER_PARAM_" (kompatibel zu JobScheduler 1), 
    außerdem mit Vorrang die Auftragsvariablen.   

Die Anweisung ```executable``` kann auch direkt im Workflow verwendet werden:
```
define workflow {
  execute executable="/test-C", agent="agent-1";
}
```

  
### Konfiguration im Agenten

Statt der Jobs erwartet der Agent bloß die zugelassenen Programme in seinem Konfigurationsverzeichnis ```config/executables```. 
Unterverzeichnisse und symbolische Links sind möglich.
Unter Unix muss das _executable_-Bit gesetzt sein (chmod +x).
