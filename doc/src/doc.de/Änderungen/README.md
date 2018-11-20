# Änderungen

## 2018-10-20

Ein Workflow wird jetzt definiert mit ```define workflow { ... }```, zum Beispiel:

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

**Neue Anweisungen**

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

  
**Konfiguration im Agenten**

Statt der Jobs erwartet der Agent bloß die zugelassenen Programme in seinem Konfigurationsverzeichnis ```config/executables```. 
Unterverzeichnisse und symbolische Links sind möglich.
Unter Unix muss das _executable_-Bit gesetzt sein (chmod +x).
