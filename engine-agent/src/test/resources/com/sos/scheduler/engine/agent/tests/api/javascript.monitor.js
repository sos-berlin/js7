function spooler_process_before(){
	spooler_log.info("##this is spooler_process_before##");
	return true;
}

function spooler_process_after(spooler_process_result){
    spooler_log.info("##this is spooler_process_after##");
	return true;
}

function spooler_task_before() {
	spooler_log.info("##this is spooler_task_before##");
	return true;
}


function spooler_task_after(){
	spooler_log.info("##this is spooler_task_after##");
}