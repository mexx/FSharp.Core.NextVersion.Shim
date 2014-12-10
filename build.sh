#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net
  cd regression

  .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  cd ..

  regression/packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
else
  cd regression

  # use mono
  mono .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  cd ..

  mono regression/packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi
