package com.sos.scheduler.engine.data.filebased;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import com.sos.scheduler.engine.data.folder.FolderPath$;
import com.sos.scheduler.engine.data.job.JobPath$;
import com.sos.scheduler.engine.data.jobchain.JobChainPath$;
import com.sos.scheduler.engine.data.lock.LockPath$;
import com.sos.scheduler.engine.data.monitor.MonitorPath$;
import com.sos.scheduler.engine.data.order.OrderKey$;
import com.sos.scheduler.engine.data.processclass.ProcessClassPath$;
import com.sos.scheduler.engine.data.schedule.SchedulePath$;
import spray.json.JsonFormat;

public enum FileBasedType {
    Folder      (FolderPath$      .MODULE$, "Folder"        , "folder"       , "Folder"      ),
    Job         (JobPath$         .MODULE$, "Job"           , "job"          , "Job"         ),
    JobChain    (JobChainPath$    .MODULE$, "Job_chain"     , "job_chain"    , "JobChain"    ),
    Lock        (LockPath$        .MODULE$, "Lock"          , "lock"         , "Lock"        ),
    Monitor     (MonitorPath$     .MODULE$, "Monitor"       , "monitor"      , "Monitor"     ),
    Order       (OrderKey$        .MODULE$, "Standing_order", "order"        , "Order"       ),
    ProcessClass(ProcessClassPath$.MODULE$, "Process_class" , "process_class", "ProcessClass"),
    Schedule    (SchedulePath$    .MODULE$, "Schedule"      , "schedule"     , "Schedule"    ),
    Unknown     (UnknownTypedPath$.MODULE$, "Unknown"       , "unknown"      , "Unknown"     );

    private final TypedPath.Companion<? extends TypedPath> companion;
    private final String internalCppName;
    private final String cppName;
    private final String camelName;
    private final String lowerCaseCamelName;

    FileBasedType(TypedPath.Companion<? extends TypedPath> companion, String internalCppName, String cppName, String camelName) {
        this.companion = companion;
        this.internalCppName = internalCppName;
        this.cppName = cppName;
        this.camelName = camelName;
        this.lowerCaseCamelName = camelName.substring(0, 1).toLowerCase() + camelName.substring(1);
    }

    public final TypedPath toPath(String path) {
        return (TypedPath)companion.apply(path);
    }

    public TypedPath.Companion<? extends TypedPath> companion() {
        return companion;
    }

    public String internalCppName() {
        return internalCppName;
    }

    public String cppName() {
        return cppName;
    }

    public String filenameExtension() {
        return this == Folder? "/" : "." + cppName + ".xml";
    }

    public String camelName() {
        return camelName;
    }

    public String lowerCaseCamelName() {
        return lowerCaseCamelName;
    }

    @Override public String toString() {
        return camelName;
    }

    public static FileBasedType fromCppName(String name) {
        for (FileBasedType o: values())
            if (o.cppName.equals(name))
                return o;
        throw new RuntimeException("Unknown file based type '"+name+"'");
    }

    public static FileBasedType fromInternalCppName(String name) {
        for (FileBasedType o: values())
            if (o.internalCppName.equals(name))
                return o;
        throw new RuntimeException("Unknown file based type '"+name+"'");
    }

    public static final JsonFormat<FileBasedType> MyJsonFormat = new JavaEnumJsonFormat<>(FileBasedType.class);
}
