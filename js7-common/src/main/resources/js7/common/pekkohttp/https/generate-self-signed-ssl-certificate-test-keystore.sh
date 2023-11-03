#!/usr/bin/env bash
set -euo pipefail

# Generates a test TLS certificate
# Tested only with -host=localhost.

# The source code test keystores has been generated with:
#   common/src/main/resources/js7/common/pekkohttp/https/generate-self-signed-ssl-certificate-test-keystore.sh --host=localhost --alias=test --config-directory=common/src/test/resources/js7/common/pekkohttp/https
#   common/src/main/resources/js7/common/pekkohttp/https/generate-self-signed-ssl-certificate-test-keystore.sh --host=localhost --alias=agent --config-directory=agent/src/test/resources/js7/agent/test/config

configDirectory=
host=
distinguishedName=
days=36500
prefix=""
alias=
keyPassword="jobscheduler"
storePassword="jobscheduler"

for arg in "$@"; do
  case "$arg" in
    --alias=*)
      alias="${arg#*=}"
      ;;
    --host=*)
      host="${arg#*=}"
      ;;
    --distinguished-name=*)
      distinguishedName="${arg#*=}"
      ;;
    --config-directory=*)
      configDirectory="${arg#*=}"
      ;;
    --days=*)
      days="${arg#*=}"
      ;;
    --prefix=*)
      prefix="${arg#*=}"
      ;;
    *)
      echo Unknown argument: $arg
      exit 1
  esac
done

test -n "$distinguishedName" || {
  echo "--distinguished-name should not be empty"
  exit 1
}

keyStore="private/${prefix}https-keystore.p12"
trustStore="export/${prefix}https-truststore.p12"
trustPem="export/${prefix}https-truststore.pem"

[ -n "$host" ] || {
  echo Missing argument for -host=
  exit 1
}
[ -n "$alias" ] || {
  echo Missing argument for -alias=
  exit 1
}
[ -n "$configDirectory" ] || {
  echo Missing argument for -config-directory=
  exit 1
}
cd "$configDirectory"

mkdir -p "${trustStore%/*}"
rm -vf "$keyStore" "$trustStore" "$trustPem"

keytool -genkey \
  -alias "$alias" \
  -dname "$distinguishedName" \
  -ext "san=dns:$host" \
  -validity "$days" \
  -keyalg RSA \
  -keysize 1024 \
  -keypass "$keyPassword" \
  -keystore "$keyStore" \
  -storetype pkcs12 \
  -storepass "$storePassword"

echo "----------------------------------------------------------------------"
echo "File $keyStore:"
keytool -list -keystore "$keyStore" -deststorepass "$storePassword"

keytool -exportcert -rfc -noprompt \
  -keystore "$keyStore" \
  -storepass "$storePassword" \
  -alias "$alias" \
  -file "$trustPem"
keytool -importcert -noprompt \
  -file "$trustPem"  \
  -keystore "$trustStore" \
  -storetype pkcs12 \
  -storepass "$storePassword" \
  -alias "$alias"

echo "----------------------------------------------------------------------"
echo "File $trustStore:"
keytool -list \
  -keystore "$trustStore" \
  -deststorepass "$storePassword"
echo "----------------------------------------------------------------------"
echo "File $trustPem:"
keytool -printcert \
  -file "$trustPem"
