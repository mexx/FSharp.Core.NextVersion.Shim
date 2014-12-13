# FSharp.Core.NextVersion.Shim

This project provides a shim for library functions that will get available in the next FSharp.Core library.

## How to get

Reference with [Paket](http://fsprojects.github.io/Paket/)
```
github mexx/FSharp.Core.NextVersion.Shim FSharp.Core.Shim.fs
```

## What's inside

* Regularized collection functions, like `List.groupBy`, for full list see [Regular functional functions](https://visualfsharp.codeplex.com/wikipage?title=Status)  section
* `WebClient.AsyncDownloadFile` and `WebClient.AsyncDownloadData`
* `String.filter` and `Option.filter`

## Build status

|  |  BuildScript | Status of last build |
| :------ | :------: | :------: |
| **Mono** | [build.sh](regression/build.sh) | [![Travis build status](https://travis-ci.org/mexx/FSharp.Core.NextVersion.Shim.svg?branch=master)](https://travis-ci.org/mexx/FSharp.Core.NextVersion.Shim) |
| **Windows** | [build.cmd](regression/build.cmd) | [![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/6bcn6329xiyx9rw0/branch/master?svg=true)](https://ci.appveyor.com/project/mexx/fsharp-core-nextversion-shim/branch/master) |

## License

The [Apache License, Version 2.0](LICENSE.txt)
