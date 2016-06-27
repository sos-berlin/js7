#! /bin/bash
# Generates a test TLS certificate
set -e

distinguishedName="CN=localhost, O=Test"
days=3660
privateKeystore="config/private/private-https.jks"
publicKeystore="agent-https.jks"
alias=agent-https
keyPassword="jobscheduler"
storePassword="jobscheduler"
publicCertFile="agent-https.pem"

for arg in "$@"; do
    case $arg in
        -distinguished-name=*)
            distinguishedName="${arg#*=}"
            shift
            ;;
        -data-directory=*)
            cd "${arg#*=}"
            shift
            ;;
        -resources)
            # Generate test code files (to be called manually)
            cd "$(dirname "$0")"
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

rm -v --force "$privateKeystore" "$publicKeystore" "$publicCertFile"

keytool -genkey \
  -alias "$alias" \
  -dname "$distinguishedName" \
  -validity "$days" \
  -keyalg RSA \
  -keysize 1024 \
  -keypass "$keyPassword" \
  -keystore "$privateKeystore" \
  -storepass "$storePassword"

echo $(readlink --canonicalize $privateKeystore)
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

keytool -list \
  -keystore "$publicKeystore" \
  -deststorepass "$storePassword"
echo
echo $distinguishedName
readlink --canonicalize "$privateKeystore" "$publicKeystore" "$publicCertFile"
