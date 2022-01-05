version = "0.1.0"
author = "creator_name"
description = "Tests nimble syntax highlighting"
license = "custom"

bin = @["test"]
srcdir = "src"
installExt = @["nim"]

# Dependencies

requires "nim >= 1.6.2"

when defined(nimdistros):
  import distros
  if detectOs(Ubuntu):
    foreignDep "external_lib"
  else:
    foreignDep "other_lib"

task test, "Runs a task called 'test'":
  withDir "tests":
    exec "nim c -r tester"
