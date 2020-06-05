#!/usr/bin/env bash
set -e

# Generates a test TLS certificate
# Tested only with -host=localhost.

# The source code test keystores has been generated with:
#   common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=test -config-directory=common/src/test/resources/js7/common/akkahttp/https
#   common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=agent -config-directory=agent/src/test/resources/js7/agent/test/config

configDirectory=
host=
distinguishedName=
days=36500
keyStore="private/https-keystore.p12"
trustStore="export/https-truststore.p12"
trustPem="export/https-truststore.pem"
alias=
keyPassword="jobscheduler"
storePassword="jobscheduler"

for arg in "$@"; do
  case "$arg" in
    -alias=*)
      alias="${arg#*=}"
      shift
      ;;
    -host=*)
      host="${arg#*=}"
      distinguishedName="CN=$host"
      shift
      ;;
    -distinguished-name=*)
      distinguishedName="${arg#*=}"
      shift
      ;;
    -config-directory=*)
      configDirectory="${arg#*=}"
      shift
      ;;
    -days=*)
      days="${arg#*=}"
      shift
      ;;
    *)
      echo Unknown argument: $arg
      exit 1
      ;;
  esac
done

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
  -validity "$days" \
  -keyalg RSA \
  -keysize 1024 \
  -keypass "$keyPassword" \
  -keystore "$keyStore" \
  -storetype pkcs12 \
  -storepass "$storePassword"

echo "----------------------------------------------------------------------"
echo File $keyStore:
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
echo File $trustStore:
keytool -list \
  -keystore "$trustStore" \
  -deststorepass "$storePassword"
echo "----------------------------------------------------------------------"
echo File $trustPem:
keytool -printcert \
  -file "$trustPem"
