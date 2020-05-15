// This test should be considered a failure if the detected syntax differs between the following two commands:
/*
# bat --map-syntax '*.js:Markdown' --file-name 'issue_985.js' < issue_985.js
# bat --map-syntax '*.js:Markdown' --file-name 'issue_985.js' issue_985.js
*/
