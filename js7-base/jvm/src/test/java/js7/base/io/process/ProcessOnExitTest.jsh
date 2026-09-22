//usr/bin/env jshell "$0"; exit $?

// Small test case for blocking onExit

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import static java.lang.System.nanoTime;

var numberOfProcesses = 100;
var backgroundDuration = 5.0;

long t0 = 0;

double elapsed() {
    return (nanoTime() - t0) / 1e9;
}

void drain(InputStream in) {
    try {
        in.readAllBytes();
    } catch (IOException e) {
        throw new RuntimeException(e);
    }
}

var scriptFile = Files.createTempFile("ProcessOnExitTest-", ".sh");
Files.writeString(scriptFile, "sleep " + backgroundDuration + " &\nsleep 1\n");
Files.setPosixFilePermissions(scriptFile, PosixFilePermissions.fromString("r-x------"));
t0 = nanoTime();
var done = new CountDownLatch(numberOfProcesses);
for (int i = 0; i < numberOfProcesses; i++) {
    var process = new ProcessBuilder(scriptFile.toString()).start();
    process.getOutputStream().close();
    process.toHandle().onExit()
        .thenAccept(x -> {
            // onExit should be completed 1s after shell start and before backgroundDuration
            var e = elapsed();
            System.out.printf("%.1fs%s ", e, e < backgroundDuration ? "" : "❌");
            done.countDown();
        });

    // This line may block onExit.thenAccept of some processs:
    Thread.ofVirtual().start(() -> drain(process.getInputStream()));
}
done.await(30, TimeUnit.SECONDS);
System.out.println();
Files.delete(scriptFile);

/exit
