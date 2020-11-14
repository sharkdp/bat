-- This is a comment

property defaultClientName : "Mary Smith"

on greetClient(nameOfClient)
    display dialog ("Hello " & nameOfClient & "!")
end greetClient


script testGreet
    greetClient(defaultClientName)
end script

run testGreet
greetClient("Joe Jones")

set myList to {1, "what", 3}
set beginning of myList to 0
set end of myList to "four"

myList

tell application "TextEdit"
    paragraph 1 of document 1
end tell
