namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Core.NextVersion.Shim")>]
[<assembly: AssemblyProductAttribute("FSharp.Core.NextVersion.Shim")>]
[<assembly: AssemblyDescriptionAttribute("Shim for functionality added to next FSharp.Core library version but not published yet")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
