#! /bin/bash
# Generates a test TLS certificate
set -e

directory=""
distinguishedName="CN=localhost, O=Test"
days=3660
privateKeystore="config/private/private-https.jks"
publicKeystore="public-https.jks"
alias=""
keyPassword="jobscheduler"
storePassword="jobscheduler"
publicCertFile="public-https.pem"

for arg in "$@"; do
    case $arg in
        -alias=*)
            alias="${arg#*=}"
            shift
            ;;
        -distinguished-name=*)
            distinguishedName="${arg#*=}"
            shift
            ;;
        -data-directory=*)
            directory="${arg#*=}"
            shift
            ;;
        -resources)
            # Generate test code files (to be called manually)
            directory="$(dirname "$0")"
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

[ -n "$alias" ] || {
    echo Missing argument for -alias=
    exit 1
}
[ -n "$directory" ] || {
    echo Missing argument for -data-directory=
    exit 1
}
cd "$directory"

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
