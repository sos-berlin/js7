#! /bin/bash
# Generates a test TLS certificate with openssl
set -e

echo "NOT UP-TO-DATE !  USE generate-self-signed-ssl-certificate-test-keystore"
exit 1

# Origin: https://stackoverflow.com/questions/17695297/importing-the-private-key-public-certificate-pair-in-the-java-keystore
# This script file is expected in src/main/scripts.
if [ -d tmp ]; then
  rm --force tmp/*
  rmdir tmp
fi
mkdir tmp

javaKeyStore="https.jks"
rm -v --force $javaKeyStore

echo "-- Generate key with AES256"
openssl genrsa -aes256 \
  -out tmp/server.key \
  -passout "pass:server.key" \
  1024

echo "-- Generate certificate request for CA"
openssl req -batch -x509 -sha256 -new \
  -key tmp/server.key \
  -passin "pass:server.key" \
  -out tmp/server.csr

echo "-- Generate self-signed certificate, valid for 10 years"
openssl x509 -sha256 \
  -days 3652 \
  -in tmp/server.csr \
  -signkey tmp/server.key \
  -passin "pass:server.key" \
  -out tmp/selfsigned.crt

echo "-- Create PKCS12 keystore from private key and public certificate"
openssl pkcs12 -export \
  -in tmp/selfsigned.crt \
  -inkey tmp/server.key \
  -passin "pass:server.key" \
  -name ssl-server-certificate \
  -out tmp/https.p12 \
  -passout "pass:https.p12"

echo "-- Convert PKCS12 keystore into a JKS keystore"
keytool -importkeystore -noprompt \
  -srcstoretype pkcs12 \
  -srckeystore tmp/https.p12 \
  -alias ssl-server-certificate \
  -srcstorepass "jobscheduler" \
  -destkeystore https.jks \
  -deststorepass "jobscheduler"
rm -v tmp/server.key tmp/server.csr tmp/selfsigned.crt tmp/https.p12

keytool -list -keystore "$javaKeyStore" -deststorepass "jobscheduler"
echo $(readlink --canonicalize $javaKeyStore)
