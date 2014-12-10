@echo off
cls

cd regression

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

cd ..

regression\packages\FAKE\tools\FAKE.exe build.fsx %*
