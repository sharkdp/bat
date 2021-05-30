*** Settings ***
Documentation     PROBLEM:
...               You want to test the existence of a file in an AWS S3 bucket
...               without using lower level Python code or developing a custom library.
...               DISCUSSION:
...               This recipe demonstrates:
...               - using a Suite Teardown to end the test suite cleanly
...               - using keywords from an external library
...               - accessing OS Environment Variables directly using %{} syntax
...               This recipe has the following external dependencies:
...               $ pip install --upgrade robotframework-aws
...               This recipe also requires the following OS environment variables:
...               AWS_ACCESS_KEY_ID
...               AWS_SECRET_ACCESS_KEY
Suite Teardown    Delete All Sessions
Library           AWSLibrary
Force Tags        no-ci-testing

*** Variables ***
${recipe}         Recipe 14.1 AWS Simple Storage Service
${level}          Intermediate
${category}       External Library: AWSLibrary
${REGION}         us-east-1
${BUCKET}         YOUR_BUCKET_NAME_GOES_HERE
${KEY}            YOUR_FILE_PATH_GOES_HERE

*** Test Cases ***
Check Key Exists In Bucket
    Log Variables
    Create Session With Keys    ${REGION}    %{AWS_ACCESS_KEY_ID}    %{AWS_SECRET_ACCESS_KEY}
    Key Should Exist            ${BUCKET}    ${KEY}
