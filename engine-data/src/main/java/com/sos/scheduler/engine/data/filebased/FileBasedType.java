package com.sos.scheduler.engine.data.filebased;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import com.sos.scheduler.engine.data.folder.FolderPath;
import com.sos.scheduler.engine.data.job.JobPath;
import com.sos.scheduler.engine.data.jobchain.JobChainPath;
import com.sos.scheduler.engine.data.lock.LockPath;
import com.sos.scheduler.engine.data.monitor.MonitorPath;
import com.sos.scheduler.engine.data.order.OrderKey;
import com.sos.scheduler.engine.data.processclass.ProcessClassPath;
import com.sos.scheduler.engine.data.schedule.SchedulePath;
import spray.json.JsonFormat;

public enum FileBasedType {
    folder("Folder", "folder", "Folder") {
        public FolderPath toPath(String o) {
            return new FolderPath(o);
        }
    },
    
    job("Job", "job", "Job") {
        public JobPath toPath(String o) {
            return new JobPath(o);
        }
    },

    jobChain("Job_chain", "job_chain", "JobChain") {
        public JobChainPath toPath(String o) {
            return new JobChainPath(o);
        }
    },
    
    lock("Lock", "lock", "Lock") {
        public LockPath toPath(String o) {
            return new LockPath(o);
        }
    },
    
    monitor("Monitor", "monitor", "Monitor") {
        public MonitorPath toPath(String o) {
            return new MonitorPath(o);
        }
    },

    order("Standing_order", "order", "Order") {
        public OrderKey toPath(String o) {
            return OrderKey.apply(o);
        }
    },
    
    processClass("Process_class", "process_class", "ProcessClass") {
        public ProcessClassPath toPath(String o) {
            return new ProcessClassPath(o);
        }
    },
    
    schedule("Schedule", "schedule", "Schedule") {
        public SchedulePath toPath(String o) {
            return new SchedulePath(o);
        }
    };

    private final String internalCppName;
    private final String cppName;
    private final String printName;

    FileBasedType(String internalCppName, String cppName, String printName) {
        this.internalCppName = internalCppName;
        this.cppName = cppName;
        this.printName = printName;
    }
    
    public abstract TypedPath toPath(String path);

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
