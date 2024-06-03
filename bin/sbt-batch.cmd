@echo off
setlocal

rem set SBT_OPTS=
set JAVA_OPTS=-Xmx2500M -XX:MaxMetaspaceSize=1500M -Dlog4j2.configurationFile=project/log4j2.xml -Dsbt.log.noformat=true -enableassertions %JAVA_OPTS%
sbt.bat --supershell=never --no-colors %*
endlocal
