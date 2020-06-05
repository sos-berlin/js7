FROM openjdk:15

# UID of the 'jobscheduler'. The mounted volume /var/opt/jobscheduler must be rwx for this UID
# Used as group ID, too.

# Directory structure according to the Filesystem Hierarchy Standard, https://refspecs.linuxfoundation.org/FHS_3.0/fhs-3.0.pdf
#   /opt/jobscheduler-VERSION/bin/
#   /opt/jobscheduler-VERSION/lib/
#   /opt/jobscheduler -> jobscheduler-VERSION
#   /var/opt/jobscheduler/{master,agent}/config/  read-only
#   /var/opt/jobscheduler/{master,agent}/data/{logs,state,tmp,...}  writable
#   /home/jobscheduler/bin

ARG UID

CMD "/bin/bash"
ENV LANG=en_US.UTF-8

# A tiny improvement of the Java runtime:
#RUN $JAVA_HOME/bin/jlink --no-header-files --no-man-pages \
#    --add-modules java.base,java.compiler,java.desktop,java.logging,java.management,java.security.jgss,java.sql,java.xml,jdk.management,jdk.unsupported \
#    --output /opt/jobscheduler-java
#ENV JAVA_HOME /opt/jobscheduler-java

# To find libjdwp.so for -debug-port=
ENV LD_LIBRARY_PATH "$JAVA_HOME/lib"

ADD files /
ADD jobscheduler-*install-[0-9]*.tgz /opt/

RUN chown root:root -R /opt/jobscheduler-[1-9]* && \
    chmod o-w -R /opt/jobscheduler-[1-9]* && \
    ln -s /opt/jobscheduler-[1-9]* /opt/jobscheduler && \
    groupadd --gid ${UID:-1000} jobscheduler && \
    useradd --uid ${UID:-1000} --gid jobscheduler --home-dir /home/jobscheduler --no-create-home --shell /bin/bash jobscheduler && \
    mkdir -p /var/opt/jobscheduler/testMasterAgent && \
    chown jobscheduler:jobscheduler -R /home/jobscheduler /var/opt/jobscheduler

# ~jobscheduler/bin contains scripts master and agent, useable for "docker run".
ENV PATH /home/jobscheduler/bin:/usr/local/bin:/usr/bin:/bin
USER jobscheduler

# Possible start:
# docker run --publish=4444:4444 -ti --tag=jobscheduler-agent jobscheduler agent
