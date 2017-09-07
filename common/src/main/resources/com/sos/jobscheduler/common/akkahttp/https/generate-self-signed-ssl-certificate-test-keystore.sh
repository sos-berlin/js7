#!/usr/bin/env bash
# Generates a test TLS certificate
set -e

# Tested only with -host=localhost. Then "-ext SAN=IP:127.0.0.1" is added to the certificate.

# The source code test keystores has been generated with:
# common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=test -config-directory=agent/src/main/resources/com/sos/jobscheduler/agent/test/config

configDirectory=
host=
distinguishedName=
days=3660
privateKeystore="private/private-https.jks"
publicKeystore="public-https.jks"
alias=
keyPassword="jobscheduler"
storePassword="jobscheduler"
publicCertFile="public-https.pem"

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

rm -vf "$privateKeystore" "$publicKeystore" "$publicCertFile"

keytool -genkey \
  -alias "$alias" \
  -dname "$distinguishedName" \
  -ext SAN=DNS:$host \
  $([ "$host" != "localhost" ] || echo "-ext SAN=IP:127.0.0.1") \
  -validity "$days" \
  -keyalg RSA \
  -keysize 1024 \
  -keypass "$keyPassword" \
  -keystore "$privateKeystore" \
  -storepass "$storePassword"

echo "----------------------------------------------------------------------"
echo File $privateKeystore:
keytool -list -keystore "$privateKeystore" -deststorepass "$storePassword"

keytool -exportcert -rfc -noprompt \
  -keystore "$privateKeystore" \
  -storepass "$storePassword" \
  -alias "$alias" \
  -file "$publicCertFile"
keytool -importcert -noprompt \
  -file "$publicCertFile" \
  -keystore "$publicKeystore" \
  -storepass "$storePassword" \
  -alias "$alias"

echo "----------------------------------------------------------------------"
echo File $publicKeystore:
keytool -list \
  -keystore "$publicKeystore" \
  -deststorepass "$storePassword"
echo "----------------------------------------------------------------------"
echo File $publicCertFile:
keytool -printcert \
  -file "$publicCertFile"
