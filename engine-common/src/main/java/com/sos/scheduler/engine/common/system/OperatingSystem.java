package com.sos.scheduler.engine.common.system;

import com.google.common.base.Joiner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

import static com.google.common.base.Strings.nullToEmpty;
import static com.google.common.collect.ObjectArrays.concat;

public abstract class OperatingSystem {
    public static final String name = System.getProperty("os.name");
    public static final CpuArchitecture cpuArchitecture = CpuArchitecture.cpuArchitecture();
    public static final boolean isWindows = name.startsWith("Windows");
    public static final boolean isUnix = !isWindows;
    public static final Unix unix = new Unix();
    public static final Windows windows = new Windows();
    public static final OperatingSystem operatingSystem = isWindows? windows : unix;
    public static final String javaLibraryPathPropertyName = "java.library.path";
    private static final Logger logger = LoggerFactory.getLogger(OperatingSystem.class);
    
    public final String makeModuleFilename(String path) {
        File file = new File(path);
        return new File(file.getParent(), System.mapLibraryName(file.getName())).getPath();
    }
    
    public abstract String makeExecutableFilename(String name);
    public abstract String getDynamicLibraryEnvironmentVariableName();
//    protected abstract void removeAbsoluteDirectoryRecursivly(File directory);
//
//    public void removeDirectoryRecursivly(File directory) {
//        try {
//            File dir = directory.getCanonicalFile();
//            checkArgument(dir.isDirectory(), "Not a directory: %s", directory);
//            removeAbsoluteDirectoryRecursivly(dir);
//            if (directory.exists())  throw new RuntimeException("Removal of directory failed: " + directory);
//        } catch (IOException x) { throw new RuntimeException(x); }
//    }
//
//    public void executeShell(File program, String... arguments) {
//        try {
//            checkArgument(program.isFile(), "Not a file: %s", program);
//            String[] commandAndArguments = ObjectArrays.concat(new String[]{program.toString()}, arguments, String.class);
//            if (log.isDebugEnabled()) log.debug("exec " + Joiner.on(' ').join(commandAndArguments));
//            Process process = Runtime.getRuntime().exec(commandAndArguments);
//            *** Hier muss vermutlich process.getOutputStream() und .getErrorStream() ausgelesen werden, um Blockade zu vermeiden ***
//            process.waitFor();
//            if (process.exitValue() != 0)
//                throw new RuntimeException("Execution of program failed with exit code " + process.exitValue() + ": " + program);
//        } catch (IOException x) { throw new RuntimeException(x); }
//        catch (InterruptedException x) { throw new RuntimeException(x); }
//    }
    

    public static class Windows extends OperatingSystem {
        @Override public final String makeExecutableFilename(String name) {
            return name + ".exe";
        }

        @Override public final String getDynamicLibraryEnvironmentVariableName() {
            return "PATH";
        }

//        @Override public void removeAbsoluteDirectoryRecursivly(File directory) {
//            executeShell(commandFile(), "rd", "/s", directory.toString());
//        }
//
//        private File commandFile() {
//            return new File(windowsDirectory() + File.separator + "system32", "cmd.exe");
//        }
//
//        private File windowsDirectory() {
//            String windir = System.getenv("WINDIR");
//            if (isNullOrEmpty(windir))  throw new RuntimeException("Environment variable WINDIR is missing");
//            File result = new File(windir);
//            assert result.isDirectory();
//            return result;
//        }
    }
    

    public static class Unix extends OperatingSystem {
        @Override public final String makeExecutableFilename(String name) {
            return name;
        }

        @Override public final String getDynamicLibraryEnvironmentVariableName() {
            return "LD_LIBRARY_PATH";
        }

//        @Override public void removeAbsoluteDirectoryRecursivly(File directory) {
//            executeShell(new File("/bin/rm"), "-rf", directory.toString());
//        }
    }

    public static void prependJavaLibraryPath(File f) {
        String a = System.getProperty(javaLibraryPathPropertyName);
        String c = concatFileAndPathChain(f, nullToEmpty(a));
        if (!c.equals(a)) {
            System.setProperty(javaLibraryPathPropertyName, c);
            logger.debug("Property " + javaLibraryPathPropertyName + "=" + c);
        }
    }

    public static String concatFileAndPathChain(File f, String pathChain) {
        String abs = f.getAbsolutePath();
        String[] b = pathChain.split(File.pathSeparator);
        for (int i = 0; i < b.length; i++)  if (b[i].isEmpty() || b[i].equals(abs))  b[i] = null;
        return Joiner.on(File.pathSeparatorChar).skipNulls().join(concat(abs, b));
    }
}
