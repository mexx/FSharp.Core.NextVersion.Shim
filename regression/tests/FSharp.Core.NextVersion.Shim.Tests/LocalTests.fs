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
