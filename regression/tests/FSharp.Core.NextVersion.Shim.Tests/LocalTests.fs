namespace LocalTests

open NUnit.Framework

open FSharp.Core.Unittests.LibraryTestFx

[<TestFixture>]
type SeqModule() =

    [<Test>]
    member this.foldBack2() =
        // int Seq
        let funcInt x y z = x + y + z
        let intSeq = seq { 1..10 }
        let resultInt = Seq.foldBack2 funcInt intSeq (seq { 1..2..20 }) 9
        Assert.AreEqual(164, resultInt)

        // string Seq
        let funcStr = sprintf "%s%s%s"
        let strSeq = seq [ "A"; "B"; "C"; "D" ]
        let resultStr = Seq.foldBack2  funcStr strSeq (seq [ "a"; "b"; "c"; "d"]) "*"
        Assert.AreEqual("AaBbCcDd*", resultStr)

        // single element
        let strSeqSingle = seq [ "X" ]
        Assert.AreEqual("XAZ", Seq.foldBack2 funcStr strSeqSingle strSeq "Z")
        Assert.AreEqual("AXZ", Seq.foldBack2 funcStr strSeq strSeqSingle "Z")
        Assert.AreEqual("XYZ", Seq.foldBack2 funcStr strSeqSingle (seq [ "Y" ]) "Z")

        // empty Seq
        let emptySeq = Seq.empty
        Assert.AreEqual(1, Seq.foldBack2 funcInt emptySeq emptySeq 1)
        Assert.AreEqual(1, Seq.foldBack2 funcInt emptySeq intSeq 1)
        Assert.AreEqual(1, Seq.foldBack2 funcInt intSeq emptySeq 1)

        // infinite Seq
        let infiniteSeq = Seq.initInfinite (fun i -> 2 * i + 1)
        Assert.AreEqual(164, Seq.foldBack2 funcInt intSeq infiniteSeq 9)
        Assert.AreEqual(164, Seq.foldBack2 funcInt infiniteSeq intSeq 9)

        // null Seq
        let nullSeq:seq<'a> = null
        CheckThrowsArgumentNullException (fun () -> Seq.foldBack2 funcInt nullSeq intSeq 1 |> ignore)
        CheckThrowsArgumentNullException (fun () -> Seq.foldBack2 funcInt intSeq nullSeq 1 |> ignore)
        CheckThrowsArgumentNullException (fun () -> Seq.foldBack2 funcInt nullSeq nullSeq 1 |> ignore)

        ()

[<TestFixture>]
type UnboxAndOptionStuff() =

    [<Test>]
    member this.TryUnbox() =
        Assert.IsTrue( tryUnbox (box ([] : int list)) = Some ([]: int list))
        Assert.IsTrue( tryUnbox (box ([1] : int list)) = Some ([1]: int list))
        Assert.IsTrue( tryUnbox (box ([] : string list)) = (None : int list option)) // Option uses 'null' as presentation
        Assert.IsTrue( tryUnbox<int list> (box ([] : string list)) = None)
        Assert.IsTrue( tryUnbox (box (None : int option)) = Some (None: int option))
        Assert.IsTrue( tryUnbox (box (None : string option)) = Some (None: string option))
        Assert.IsTrue( tryUnbox (box (None : string option)) = Some (None: int option)) // Option uses 'null' as presentation
        Assert.IsTrue( tryUnbox (box "") = Some "")
        Assert.IsTrue( tryUnbox<int option> (box null) = Some None) // Option uses 'null' as representation
        Assert.IsTrue( tryUnbox<int list> (box null) = None)
        Assert.IsTrue( tryUnbox<int> (box null) = None)
        Assert.IsTrue( tryUnbox<int> (box "1") = None)
        Assert.IsTrue( tryUnbox<int> (box 1) = Some 1)
        Assert.IsTrue( tryUnbox<string> (box "") = Some "")
        Assert.IsTrue( tryUnbox<string> (box 1) = None)

    [<Test>]
    member this.IsNull() =
        Assert.IsTrue( isNull (null : string))
        Assert.IsTrue( isNull (null : string[]))
        Assert.IsTrue( isNull (null : int[]))
        Assert.IsTrue( not (isNull [| |]))
        Assert.IsTrue( not (isNull ""))
        Assert.IsTrue( not (isNull "1"))

    [<Test>]
    member this.OptionConversions() =
        Assert.IsTrue( Option.ofNullable (System.Nullable<int>()) = None)
        Assert.IsTrue( Option.ofNullable (System.Nullable<int>(3)) = Some 3)
        Assert.IsTrue( Option.toNullable (None : int option) = System.Nullable<int>())
        Assert.IsTrue( Option.toNullable (None : System.DateTime option) = System.Nullable())
        Assert.IsTrue( Option.toNullable (Some 3) = System.Nullable(3))

        Assert.IsTrue( Option.toObj (Some "3") = "3")
        Assert.IsTrue( Option.toObj (Some "") = "")
        Assert.IsTrue( Option.toObj (Some null) = null)
        Assert.IsTrue( Option.toObj None = null)

        Assert.IsTrue( Option.ofObj "3" = Some "3")
        Assert.IsTrue( Option.ofObj "" = Some "")
        Assert.IsTrue( Option.ofObj [| "" |] = Some [| "" |])
        Assert.IsTrue( Option.ofObj (null : string array) = None)
        Assert.IsTrue( Option.ofObj<string> null = None)
        Assert.IsTrue( Option.ofObj<string[]> null = None)
        Assert.IsTrue( Option.ofObj<int[]> null = None)
