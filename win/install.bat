:: Don't show commands
@echo off

REM Check if running with administrative privileges
net session >nul 2>&1
if %errorlevel% neq 0 (
    echo Requesting administrative privileges...
    powershell -Command "Start-Process -FilePath \"%0\" -Verb RunAs"
    exit /b
)

REM Your script continues here with administrative privileges
REM Check if Scoop is installed
where scoop >nul 2>nul
if %errorlevel% neq 0 (
    REM Scoop is not installed, so install it
    powershell -Command "iwr -useb get.scoop.sh | iex"
	scoop bucket add extras

) else (
    REM Scoop is already installed, do nothing
    echo Scoop is already installed.
)

REM Check if WSL 2 is installed
@powershell -Command "Get-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux | Where-Object {$_.State -eq 'Enabled'}" >nul
if %errorlevel% neq 0 (
    REM WSL 2 is not installed, so enable it
	powershell -Command "Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux"
    set wsl_kernel_update=https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_x64.msi
    powershell -Command "Invoke-WebRequest '%wsl_kernel_update%' -OutFile '%TEMP%\wsl_update_x64.msi'"
    msiexec.exe /i %TEMP%\wsl_update_x64.msi
	powershell -Command "wsl --install"
) else (
    REM WSL 2 is already installed, do nothing
    echo WSL 2 is already installed.
)



REM Download nixos tar file and install it
set "distro_name=NixOS"
set "found=false"
wsl --list | findstr /C:"%distro_name%" > nul
if %errorlevel% equ 0 (
    echo %distro_name% is installed.
) else (
    echo %distro_name% is not installed.
)

set nixosLink=https://github.com/nix-community/NixOS-WSL/releases/download/2311.5.3/nixos-wsl.tar.gz
::curl https://github.com/nix-community/NixOS-WSL/releases/download/2311.5.3/nixos-wsl.tar.gz
::powershell -Command "Invoke-WebRequest '%nixosLink%' -OutFile '%TEMP%\nixos-wsl.tar.gz'"
wsl --import NixOS .\NixOS\ %TEMP%\nixos-wsl.tar.gz --version 2


::scoop install git jq bat fzf autohotkey
::scoop install komorebi whkd
