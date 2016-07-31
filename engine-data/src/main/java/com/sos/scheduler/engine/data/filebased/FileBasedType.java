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
    folder(FolderPath$.MODULE$, "Folder", "folder", "Folder"),
    job(JobPath$.MODULE$, "Job", "job", "Job"),
    jobChain(JobChainPath$.MODULE$, "Job_chain", "job_chain", "JobChain"),
    lock(LockPath$.MODULE$, "Lock", "lock", "Lock"),
    monitor(MonitorPath$.MODULE$, "Monitor", "monitor", "Monitor"),
    order(OrderKey$.MODULE$, "Standing_order", "order", "Order"),
    processClass(ProcessClassPath$.MODULE$, "Process_class", "process_class", "ProcessClass"),
    schedule(SchedulePath$.MODULE$, "Schedule", "schedule", "Schedule");

    private final TypedPath.Companion<?> companion;
    private final String internalCppName;
    private final String cppName;
    private final String printName;

    FileBasedType(TypedPath.Companion<?> companion, String internalCppName, String cppName, String printName) {
        this.companion = companion;
        this.internalCppName = internalCppName;
        this.cppName = cppName;
        this.printName = printName;
    }

    public final TypedPath toPath(String path) {
        return (TypedPath)companion.apply(path);
    }

    public String internalCppName() {
        return internalCppName;
    }

    public String cppName() {
        return cppName;
    }

    public String filenameExtension() {
        return this == folder ? "/" : "." + cppName + ".xml";
    }

    @Override public String toString() {
        return printName;
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
