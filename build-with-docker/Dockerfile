FROM almalinux:10

ENV LANG=C.UTF-8

ENV JAVA_HOME=/opt/java/openjdk
ENV PATH="${JAVA_HOME}/bin:${PATH}"

COPY --from=eclipse-temurin:17 $JAVA_HOME $JAVA_HOME

# Own Docker image layer with downloaded microdnf metadata (may take several seconds)
RUN yum -y install procps git unzip which sudo openssl
# Until here, same Dockerfile command sequence as in JS7 Docker example for shorter build time
#———————————————————————————————————————————————————————————————————————————————

RUN <<END
  # Install packages
  set -euo pipefail
  yum -y install nodejs
  yum clean packages
END

RUN <<END
  # Install sbt
  set -euo pipefail
  sbtVersion=1.11.7
  # Get a changed version's hash code with this command:
  # curl --silent --show-error --location https://github.com/sbt/sbt/releases/download/v1.11.7/sbt-1.11.7.tgz | sha256sum
  sbtSHA256=1232818f91c39639a93bbe1108e12d94c7044a646a7847f1a3977b9e46716cd6
  mkdir --parents /opt/lib/sbt-$sbtVersion
  cd /root
  curl --location https://github.com/sbt/sbt/releases/download/v$sbtVersion/sbt-$sbtVersion.tgz \
       --output sbt.tgz
  echo "$sbtSHA256 sbt.tgz" | sha256sum --check
  tar xzf sbt.tgz --directory=/opt/lib/sbt-$sbtVersion --strip-components=1 sbt/
  rm sbt.tgz
  ln -snf /opt/lib/sbt-$sbtVersion/bin/sbt /usr/local/bin/sbt
END

RUN <<END
  # Finish
  set -euo pipefail
  adduser --comment "JS7 build" --no-create-home build
  mkdir --parents /home/build/mount/{.cache,.ivy2,.npm,.sbt}
  ln -s /home/build/mount/{.cache,.ivy2,.npm,.sbt} /home/build/
  mkdir /home/build/js7
  chown -R build.build /home/build/
END

# echo "build ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers

USER build
WORKDIR /home/build/js7
