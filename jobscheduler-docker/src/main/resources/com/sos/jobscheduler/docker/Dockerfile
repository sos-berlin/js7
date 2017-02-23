FROM openjdk:8-jre-alpine

EXPOSE 4444
CMD "/bin/bash"
RUN apk add --update bash
    #rm -rf /var/cache/apk/*

ADD files /
ADD jobscheduler-[1-9]*.tgz /opt/
RUN chown root.root -R /opt/jobscheduler-[1-9]* && \
    chmod o-w -R /opt/jobscheduler-[1-9]* && \
    ln -s /opt/jobscheduler-[1-9]* /opt/jobscheduler && \
    addgroup jobscheduler && \
    adduser -D -G jobscheduler jobscheduler && \
    cd /var/lib/jobscheduler && \
    chown jobscheduler.jobscheduler -R ~jobscheduler /var/lib/jobscheduler && \
    chmod o-w -R /var/lib/jobscheduler

# ~jobscheduler/bin contains scripts master and agent, useable for "docker run".
ENV PATH /home/jobscheduler/bin:/usr/local/bin:/usr/bin:/bin
USER jobscheduler
#VOLUME [ \
#  "/var/lib/jobscheduler/master", \
#  "/var/lib/jobscheduler/agent" ]

# Possible start:
# docker run --publish=4444:4444 -ti --tag=jobscheduler-agent jobscheduler agent
