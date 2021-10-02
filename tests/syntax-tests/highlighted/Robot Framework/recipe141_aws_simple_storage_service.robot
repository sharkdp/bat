[38;2;230;219;116m*** Settings ***[0m
[38;2;117;113;94mDocumentation     PROBLEM:[0m
[38;2;117;113;94m...               You want to test the existence of a file in an AWS S3 bucket[0m
[38;2;117;113;94m...               without using lower level Python code or developing a custom library.[0m
[38;2;117;113;94m...               DISCUSSION:[0m
[38;2;117;113;94m...               This recipe demonstrates:[0m
[38;2;117;113;94m...               - using a Suite Teardown to end the test suite cleanly[0m
[38;2;117;113;94m...               - using keywords from an external library[0m
[38;2;117;113;94m...               - accessing OS Environment Variables directly using %{} syntax[0m
[38;2;117;113;94m...               This recipe has the following external dependencies:[0m
[38;2;117;113;94m...               $ pip install --upgrade robotframework-aws[0m
[38;2;117;113;94m...               This recipe also requires the following OS environment variables:[0m
[38;2;117;113;94m...               AWS_ACCESS_KEY_ID[0m
[38;2;117;113;94m...               AWS_SECRET_ACCESS_KEY[0m
[38;2;190;132;255mSuite Teardown  [0m[38;2;248;248;242m  Delete All Sessions[0m
[38;2;190;132;255mLibrary  [0m[38;2;248;248;242m         AWSLibrary[0m
[38;2;190;132;255mForce Tags  [0m[38;2;248;248;242m      no-ci-testing[0m

[38;2;230;219;116m*** Variables ***[0m
[4;38;2;102;217;239m${recipe}[0m[38;2;248;248;242m         Recipe 14.1 AWS Simple Storage Service[0m
[4;38;2;102;217;239m${level}[0m[38;2;248;248;242m          Intermediate[0m
[4;38;2;102;217;239m${category}[0m[38;2;248;248;242m       External Library: AWSLibrary[0m
[4;38;2;102;217;239m${REGION}[0m[38;2;248;248;242m         us-east-1[0m
[4;38;2;102;217;239m${BUCKET}[0m[38;2;248;248;242m         YOUR_BUCKET_NAME_GOES_HERE[0m
[4;38;2;102;217;239m${KEY}[0m[38;2;248;248;242m            YOUR_FILE_PATH_GOES_HERE[0m

[38;2;230;219;116m*** Test Cases ***[0m
[38;2;249;38;114mCheck Key Exists In Bucket[0m
[38;2;248;248;242m    Log Variables[0m
[38;2;248;248;242m    Create Session With Keys    [0m[4;38;2;102;217;239m${REGION}[0m[38;2;248;248;242m    [0m[3;38;2;253;151;31m%{AWS_ACCESS_KEY_ID}[0m[38;2;248;248;242m    [0m[3;38;2;253;151;31m%{AWS_SECRET_ACCESS_KEY}[0m
[38;2;248;248;242m    Key Should Exist            [0m[4;38;2;102;217;239m${BUCKET}[0m[38;2;248;248;242m    [0m[4;38;2;102;217;239m${KEY}[0m
