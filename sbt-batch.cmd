@echo off
setlocal

rem For Scala.js (Subproject master-gui-browser)
npm install

rem set SBT_OPTS=
set JAVA_OPTS=-Xmx2500M -XX:MaxMetaspaceSize=1500M -XX:+CMSClassUnloadingEnabled -Dlog4j.configurationFile=project/log4j2.xml -Dsbt.log.noformat=true -enableassertions %JAVA_OPTS%
sbt.bat %*
endlocal
