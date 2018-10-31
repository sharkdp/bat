# This script takes care of packaging the build artifacts that will go in the
# release zipfile

$SRC_DIR = $PWD.Path
$STAGE = [System.Guid]::NewGuid().ToString()

Set-Location $ENV:Temp
New-Item -Type Directory -Name $STAGE
Set-Location $STAGE

$ZIP = "$SRC_DIR\$($Env:CRATE_NAME)-$($Env:APPVEYOR_REPO_TAG_NAME)-$($Env:TARGET).zip"

Copy-Item "$SRC_DIR\target\release\bat.exe" '.\'
# TODO: disabled for now, see issue #372
# Copy-Item $SRC_DIR\target\release\build\bat-*\out\_bat.ps1 '.\'

# readme and license
Copy-Item $SRC_DIR\README.md '.\'
Copy-Item $SRC_DIR\LICENSE-MIT '.\'
Copy-Item $SRC_DIR\LICENSE-APACHE '.\'

7z a "$ZIP" *

Push-AppveyorArtifact "$ZIP"

Set-Location ..
Remove-Item $STAGE -Force -Recurse
Set-Location $SRC_DIR
