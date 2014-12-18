var cnt = 0

function spooler_init() {
    spooler_log.info("spooler_init is called by " + name)
    return true
}

function spooler_exit() {
    spooler_log.info("spooler_exit is called by " + name)
    return true
}

function spooler_open() {
    spooler_log.info("spooler_open is called by " + name)
    return true
}

function spooler_close() {
    spooler_log.info("spooler_close is called by " + name)
    return true
}

function spooler_process() {
    if (cnt == 3) {
        return false
    } else {
        cnt++;
        spooler_log.info("spooler_process is called by " + name)
        return true
    }
}
