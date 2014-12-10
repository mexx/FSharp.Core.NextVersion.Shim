namespace Microsoft.FSharp.Core.CompilerServices

    open System.Threading.Tasks

    open System.Threading.Tasks

    module RuntimeHelpers =
        open System.Collections.Generic

        [<Struct; NoComparison; NoEquality>]
        type internal StructBox<'T when 'T : equality>(value:'T) = 
            member x.Value = value
            static member Comparer =
                let gcomparer = HashIdentity.Structural<'T>
                { new IEqualityComparer<StructBox<'T>> with 
                       member __.GetHashCode(v) = gcomparer.GetHashCode(v.Value)
                       member __.Equals(v1,v2) = gcomparer.Equals(v1.Value,v2.Value) }

namespace Microsoft.FSharp.Core

    [<assembly: AutoOpen("Microsoft.FSharp.Core")>]
    do()

    module internal SR =
        let keyNotFoundAlt = "An index satisfying the predicate was not found in the collection."
        let arrayWasEmpty = "The input array was empty."
        let inputSequenceEmpty = "The input sequence was empty."
        let inputMustBeNonNegative = "The input must be non-negative."
        let notEnoughElements = "The input sequence has an insufficient number of elements."
        let inputSequenceTooLong = "The input sequence contains more than one element."
        let arraysHadDifferentLengths = "The arrays have different lengths."
        let outOfRange = "The index is outside the legal range."
        let inputListWasEmpty = "The input list was empty."
        let indexOutOfBounds = "The index was outside the range of elements in the list."
        let resetNotSupported = "Reset is not supported on this enumerator."
        let enumerationNotStarted = "Enumeration has not started. Call MoveNext."
        let enumerationAlreadyFinished = "Enumeration already finished."

        let GetString(name:System.String) : System.String = name

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module String =

        let inline private emptyIfNull str =
            if str = null then "" else str

        /// <summary>Returns a new string containing only the characters of the string
        /// for which the given predicate returns "true"</summary>
        /// <param name="predicate">The function to test the input characters.</param>
        /// <param name="list">The input string.</param>
        /// <returns>A string containing only the characters that satisfy the predicate.</returns>
        [<CompiledName("Filter")>]
        let filter (predicate:char -> bool) (str:string) =
            let str = emptyIfNull str
            let res = System.Text.StringBuilder(str.Length)
            for i = 0 to str.Length - 1 do
                let ch = str.[i]
                if predicate ch then
                    res.Append(ch) |> ignore
            res.ToString()

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Option =

        /// <summary><c>filter f inp</c> evaluates to <c>match inp with None -> None | Some x -> if f x then Some x else None</c>.</summary>
        /// <param name="predicate">A function that evaluates whether the value contained in the option should remain, or be filtered out.</param>
        /// <param name="option">The input option.</param>
        /// <returns>The input if the predicate evaluates to true; otherwise, None.</returns>
        [<CompiledName("Filter")>]
        let filter f inp = match inp with None -> None | Some x -> if f x then Some x else None

namespace Local.LanguagePrimitives
    module ErrorStrings =
        open Microsoft.FSharp.Core

        let InputArrayEmptyString = SR.GetString(SR.arrayWasEmpty)
        let InputSequenceEmptyString = SR.GetString(SR.inputSequenceEmpty)
        let InputMustBeNonNegativeString = SR.GetString(SR.inputMustBeNonNegative)

namespace Microsoft.FSharp.Primitives.Basics

    module internal Array =
        open Microsoft.FSharp.Core
        open System.Collections.Generic

        // The input parameter should be checked by callers if necessary
        let inline zeroCreateUnchecked (count:int) =
            Array.zeroCreate count

        let inline fastComparerForArraySort<'t when 't : comparison> () =
            Comparer.Default

        let inline indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))

        let findBack f (array: _[]) =
            let rec loop i =
                if i < 0 then indexNotFound()
                elif f array.[i] then array.[i]
                else loop (i - 1)
            loop (array.Length - 1)

        let tryFindBack f (array: _[]) =
            let rec loop i =
                if i < 0 then None
                elif f array.[i] then Some array.[i]
                else loop (i - 1)
            loop (array.Length - 1)

        let findIndexBack f (array: _[]) =
            let rec loop i =
                if i < 0 then indexNotFound()
                elif f array.[i] then i
                else loop (i - 1)
            loop (array.Length - 1)

        let tryFindIndexBack f (array: _[]) =
            let rec loop i =
                if i < 0 then None
                elif f array.[i] then Some i
                else loop (i - 1)
            loop (array.Length - 1)

        let mapFold f acc (array : _[]) =
            match array.Length with
            | 0 -> [| |], acc
            | len ->
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let mutable acc = acc
                let res = zeroCreateUnchecked len
                for i = 0 to len - 1 do
                    let h',s' = f.Invoke(acc,array.[i])
                    res.[i] <- h'
                    acc <- s'
                res, acc

        let mapFoldBack f (array : _[]) acc =
            match array.Length with
            | 0 -> [| |], acc
            | len ->
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let mutable acc = acc
                let res = zeroCreateUnchecked len
                for i = len - 1 downto 0 do
                    let h',s' = f.Invoke(array.[i],acc)
                    res.[i] <- h'
                    acc <- s'
                res, acc

        let scanSubRight f (array : _[]) start fin initState =
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = initState
            let res = zeroCreateUnchecked (fin-start+2)
            res.[fin - start + 1] <- state
            for i = fin downto start do
                state <- f.Invoke(array.[i], state);
                res.[i - start] <- state
            res

        let stableSortWithKeysAndComparer (cFast:IComparer<'Key>) (c:IComparer<'Key>) (array:array<'T>) (keys:array<'Key>)  =
            // 'places' is an array or integers storing the permutation performed by the sort
            let places = zeroCreateUnchecked array.Length 
            for i = 0 to array.Length - 1 do 
                places.[i] <- i 
            System.Array.Sort<_,_>(keys, places, cFast)
            // 'array2' is a copy of the original values
            let array2 = (array.Clone() :?> array<'T>)

            // Walk through any chunks where the keys are equal
            let mutable i = 0
            let len = array.Length
            let intCompare = fastComparerForArraySort<int>()
            
            while i <  len do 
                let mutable j = i
                let ki = keys.[i]
                while j < len && (j = i || c.Compare(ki, keys.[j]) = 0) do 
                   j <- j + 1
                // Copy the values into the result array and re-sort the chunk if needed by the original place indexes
                for n = i to j - 1 do
                   array.[n] <- array2.[places.[n]]
                if j - i >= 2 then
                    System.Array.Sort<_,_>(places, array, i, j-i, intCompare)
                i <- j

        let stableSortInPlaceWith (comparer:'T -> 'T -> int) (array : array<'T>) =
            let len = array.Length
            if len > 1 then
                let keys = (array.Clone() :?> array<'T>)
                let comparer = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(comparer)
                let c = { new IComparer<'T> with member __.Compare(x,y) = comparer.Invoke(x,y) }
                stableSortWithKeysAndComparer c c array keys

    module internal List =
        open System.Collections.Generic
        open Local.LanguagePrimitives.ErrorStrings
        open Microsoft.FSharp.Core

        let private arrayZeroCreate = Array.zeroCreate

        let rec distinctToFreshConsTail (cons: LinkedList<_>) (hashSet:HashSet<_>) (list:'T list) =
            match list with
            | [] -> ()
            | (x::rest) ->
                if hashSet.Add(x) then
                    cons.AddLast(x) |> ignore
                    distinctToFreshConsTail cons hashSet rest
                else
                    distinctToFreshConsTail cons hashSet rest

        let distinctWithComparer (comparer: System.Collections.Generic.IEqualityComparer<'T>) (list:'T list) =
            match list with
            | [] -> []
            | [h] -> [h]
            | (x::rest) ->
                let hashSet =  System.Collections.Generic.HashSet<'T>(comparer)
                hashSet.Add(x) |> ignore
                let cons = LinkedList<'T>()
                cons.AddLast(x) |> ignore
                distinctToFreshConsTail cons hashSet rest
                cons |> Seq.toList

        let rec distinctByToFreshConsTail (cons: LinkedList<_>) (hashSet:HashSet<_>) keyf (list:'T list) =
            match list with
            | [] -> ()
            | (x::rest) ->
                if hashSet.Add(keyf x) then
                    cons.AddLast(x) |> ignore
                    distinctByToFreshConsTail cons hashSet keyf rest
                else
                    distinctByToFreshConsTail cons hashSet keyf rest

        let distinctByWithComparer (comparer: System.Collections.Generic.IEqualityComparer<'Key>) (keyf:'T -> 'Key) (list:'T list) =
            match list with
            | [] -> []
            | [h] -> [h]
            | (x::rest) ->
                let hashSet = System.Collections.Generic.HashSet<'Key>(comparer)
                hashSet.Add(keyf x) |> ignore
                let cons = LinkedList<'T>()
                cons.AddLast(x) |> ignore
                distinctByToFreshConsTail cons hashSet keyf rest
                cons |> Seq.toList

        let rec mapFoldToFreshConsTail (cons: LinkedList<'U>) (f:OptimizedClosures.FSharpFunc<'State, 'T, 'U * 'State>) acc xs =
            match xs with
            | [] -> acc
            | (h::t) ->
                let x',s' = f.Invoke(acc,h)
                cons.AddLast(x') |> ignore
                mapFoldToFreshConsTail cons f s' t

        let mapFold f acc xs =
            match xs with
            | [] -> [], acc
            | [h] ->
                let x',s' = f acc h
                [x'],s'
            | (h::t) ->
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let (x': 'c),s' = f.Invoke(acc,h)
                let cons = LinkedList<_>()
                cons.AddLast(x') |> ignore
                let s' = mapFoldToFreshConsTail cons f s' t
                (cons |> Seq.toList), s'

        let rec takeFreshConsTail (cons: LinkedList<_>) n (l:'T list) =
            if n = 0 then () else
            match l with
            | [] -> raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
            | x::xs ->
                cons.AddLast(x) |> ignore
                takeFreshConsTail cons (n - 1) xs

        let take n (l:'T list) =
            if n < 0 then invalidArg "count" InputMustBeNonNegativeString
            if n = 0 then [] else 
            match l with
            | [] -> raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
            | x::xs ->
                let cons = LinkedList<_>()
                cons.AddLast(x) |> ignore
                takeFreshConsTail cons (n - 1) xs
                cons |> Seq.toList

        let rec takeWhileFreshConsTail (cons: LinkedList<_>) p (l:'T list) =
            match l with
            | [] -> ()
            | x::xs ->
                if not (p x) then () else
                cons.AddLast(x) |> ignore
                takeWhileFreshConsTail cons p xs

        let takeWhile p (l: 'T list) =
            match l with
            | [] -> l
            | x :: ([] as nil) -> if p x then l else nil
            | x::xs ->
                if not (p x) then [] else
                let cons = LinkedList<_>()
                cons.AddLast(x) |> ignore
                takeWhileFreshConsTail cons p xs
                cons |> Seq.toList

        let rec splitAtFreshConsTail (cons: LinkedList<_>) index (l:'T list) =
            if index = 0 then
                l
            else
            match l with
            | [] -> raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
            | x :: xs ->
                cons.AddLast(x) |> ignore
                splitAtFreshConsTail cons (index - 1) xs

        let splitAt index (l:'T list) =
            if index < 0 then invalidArg "index" (SR.GetString(SR.inputMustBeNonNegative))
            if index = 0 then [], l else
            match l with
            | []  -> raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
            | [_] -> if index = 1 then l, [] else raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
            | x::xs ->
                if index = 1 then [x], xs else
                let cons = LinkedList<_>()
                cons.AddLast(x) |> ignore
                let tail = splitAtFreshConsTail cons (index - 1) xs
                cons |> Seq.toList, tail

        let rec filterToFreshConsTail (cons: LinkedList<_>) f (l:'T list) =
            match l with
            | [] -> ()
            | h::t -> 
                if f h then 
                    cons.AddLast(h) |> ignore
                    filterToFreshConsTail cons f t
                else 
                    filterToFreshConsTail cons f t

        let rec filter f (l:'T list) =
            match l with 
            | [] -> l
            | h :: ([] as nil) -> if f h then l else nil
            | h::t ->
                if f h then
                    let cons = LinkedList<_>()
                    cons.AddLast(h) |> ignore
                    filterToFreshConsTail cons f t;
                    cons |> Seq.toList
                else
                    filter f t

        let rec indexedToFreshConsTail (cons: LinkedList<_>) (xs:'T list) i =
            match xs with
            | [] -> ()
            | (h::t) ->
                cons.AddLast((i, h)) |> ignore
                indexedToFreshConsTail cons t (i+1)

        let indexed (xs:'T list) =
            match xs with
            | [] -> []
            | [h] -> [(0,h)]
            | (h::t) ->
                let cons = LinkedList<_>()
                cons.AddLast((0,h)) |> ignore
                indexedToFreshConsTail cons t 1
                cons |> Seq.toList

        let rec truncateToFreshConsTail (cons: LinkedList<_>) count (list:'T list) =
            if count = 0 then () else
            match list with
            | [] -> ()
            | h::t ->
                cons.AddLast(h) |> ignore
                truncateToFreshConsTail cons (count-1) t

        let truncate count (list:'T list) =
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            match list with
            | [] -> list
            | _ :: ([] as nil) -> if count > 0 then list else nil
            | h::t ->
                if count = 0 then []
                else
                    let cons = LinkedList<_>()
                    cons.AddLast(h) |> ignore
                    truncateToFreshConsTail cons (count-1) t
                    cons |> Seq.toList

        let rec unfoldToFreshConsTail (cons: LinkedList<_>) (f:'State -> ('T * 'State) option) s =
            match f s with
            | None -> ()
            | Some (x,s') ->
                cons.AddLast(x) |> ignore
                unfoldToFreshConsTail cons f s'

        let unfold (f:'State -> ('T * 'State) option) (s:'State) =
            match f s with
            | None -> []
            | Some (x,s') ->
                let cons = LinkedList<_>()
                cons.AddLast(x) |> ignore
                unfoldToFreshConsTail cons f s'
                cons |> Seq.toList

        let rec windowedToFreshConsTail (cons: LinkedList<_>) windowSize i (l:'T list) (arr:'T[]) =
            match l with
            | [] -> ()
            | h::t ->
                arr.[i] <- h
                let i = (i+1) % windowSize
                let result = arrayZeroCreate windowSize : 'T[]
                System.Array.Copy(arr, i, result, 0, windowSize - i)
                System.Array.Copy(arr, 0, result, windowSize - i, i)
                cons.AddLast(result) |> ignore
                windowedToFreshConsTail cons windowSize i t arr

        let windowed windowSize (list:'T list) =
            if windowSize <= 0 then invalidArg "windowSize" (SR.GetString(SR.inputMustBeNonNegative))
            match list with
            | [] -> []
            | _ ->
                let arr = arrayZeroCreate windowSize
                let rec loop i r l =
                    match l with
                    | [] -> if r = 0 && i = windowSize then [arr.Clone() :?> 'T[]] else []
                    | h::t ->
                        arr.[i] <- h
                        if r = 0 then
                            let cons = LinkedList<_>()
                            cons.AddLast(arr.Clone() :?> 'T[]) |> ignore
                            windowedToFreshConsTail cons windowSize 0 t arr
                            cons |> Seq.toList
                        else
                            loop (i+1) (r-1) t

                loop 0 (windowSize - 1) list

namespace Microsoft.FSharp.Collections

    [<assembly: AutoOpen("Microsoft.FSharp.Collections")>]
    do()

    module IEnumerator =
      open System.Collections
      open System.Collections.Generic
      open Microsoft.FSharp.Core

      let noReset() = raise (new System.NotSupportedException(SR.GetString(SR.resetNotSupported)))
      let notStarted() = raise (new System.InvalidOperationException(SR.GetString(SR.enumerationNotStarted)))
      let alreadyFinished() = raise (new System.InvalidOperationException(SR.GetString(SR.enumerationAlreadyFinished)))

      [<NoEquality; NoComparison>]
      type MapEnumeratorState = 
          | NotStarted 
          | InProcess 
          | Finished

      [<AbstractClass>]
      type MapEnumerator<'T> () =
          let mutable state = NotStarted
          [<DefaultValue(false)>]
          val mutable private curr : 'T
          
          member this.GetCurrent () =
              match state with
              |   NotStarted -> notStarted()
              |   Finished -> alreadyFinished()
              |   InProcess -> ()
              this.curr
          
          abstract DoMoveNext : byref<'T> -> bool
          abstract Dispose : unit -> unit
          
          interface IEnumerator<'T> with
              member this.Current = this.GetCurrent()
          
          interface IEnumerator with
              member this.Current = box(this.GetCurrent())
              member this.MoveNext () =
                  state <- InProcess
                  if this.DoMoveNext(&this.curr) then
                      true
                  else
                      state <- Finished
                      false
              member this.Reset() = noReset()
          interface System.IDisposable with
              member this.Dispose() = this.Dispose()

      let rec tryItem index (e : IEnumerator<'T>) =
          if not (e.MoveNext()) then None
          elif index = 0 then Some(e.Current)
          else tryItem (index-1) e

      let rec nth index (e : IEnumerator<'T>) = 
          if not (e.MoveNext()) then invalidArg "index" (SR.GetString(SR.notEnoughElements))
          if index = 0 then e.Current
          else nth (index-1) e

      let mapi2 f (e1 : IEnumerator<_>) (e2 : IEnumerator<_>) : IEnumerator<_> =
          let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
          let i = ref (-1)
          upcast
              {  new MapEnumerator<_>() with
                     member this.DoMoveNext curr =
                        i := !i + 1
                        if (e1.MoveNext() && e2.MoveNext()) then
                           curr <- f.Invoke(!i, e1.Current, e2.Current)
                           true
                        else
                           false
                     member this.Dispose() =
                        try
                            e1.Dispose()
                        finally
                            e2.Dispose()
              }

      let map3 f (e1 : IEnumerator<_>) (e2 : IEnumerator<_>) (e3 : IEnumerator<_>) : IEnumerator<_> = 
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        upcast 
            {  new MapEnumerator<_>() with
                   member this.DoMoveNext curr = 
                      let n1 = e1.MoveNext()
                      let n2 = e2.MoveNext()
                      let n3 = e3.MoveNext()

                      if n1 && n2 && n3 then 
                         curr <- f.Invoke(e1.Current, e2.Current, e3.Current)
                         true
                      else
                         false
                   member this.Dispose() = 
                      try
                          e1.Dispose()
                      finally
                          try
                              e2.Dispose()
                          finally
                              e3.Dispose()
            }

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        open System
        open System.Collections.Generic
        open Microsoft.FSharp.Core
        open Local
        open Local.LanguagePrimitives.ErrorStrings
        open Microsoft.FSharp.Core.CompilerServices

        let private empty = Array.empty
        let private init = Array.init
        let private filter = Array.filter
        let sortWith = Array.sortWith

        let inline private checkNonNull argName arg = 
            match box arg with 
            | null -> nullArg argName
            | _ -> ()

        /// <summary>Returns the last element for which the given function returns 'true'.
        /// Raise <c>KeyNotFoundException</c> if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="array">The input array.</param>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c>
        /// never returns true.</exception>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>The last element for which <c>predicate</c> returns true.</returns>
        [<CompiledName("FindBack")>]
        let findBack f (array: _[]) =
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.findBack f array

        /// <summary>Returns the last element for which the given function returns <c>true</c>.
        /// Return <c>None</c> if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="array">The input array.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>The last element that satisfies the predicate, or None.</returns>
        [<CompiledName("TryFindBack")>]
        let tryFindBack f (array: _[]) =
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.tryFindBack f array

        /// <summary>Returns an array that contains no duplicate entries according to generic hash and
        /// equality comparisons on the entries.
        /// If an element occurs multiple times in the array then the later occurrences are discarded.</summary>
        ///
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The result array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("Distinct")>]
        let distinct (array:'T[]) =
            checkNonNull "array" array
            let temp = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked array.Length
            let mutable i = 0

            let hashSet = HashSet<'T>(HashIdentity.Structural<'T>)
            for v in array do
                if hashSet.Add(v) then
                    temp.[i] <- v
                    i <- i + 1

            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked i : 'T[]
            Array.Copy(temp, 0, res, 0, i)
            res

        /// <summary>Returns an array that contains no duplicate entries according to the 
        /// generic hash and equality comparisons on the keys returned by the given key-generating function.
        /// If an element occurs multiple times in the array then the later occurrences are discarded.</summary>
        ///
        /// <param name="projection">A function transforming the array items into comparable keys.</param>
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The result array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("DistinctBy")>]
        let distinctBy keyf (array:'T[]) =
            checkNonNull "array" array
            let temp = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked array.Length
            let mutable i = 0 
            let hashSet = HashSet<_>(HashIdentity.Structural<_>)
            for v in array do
                if hashSet.Add(keyf v) then
                    temp.[i] <- v
                    i <- i + 1

            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked i : 'T[]
            Array.Copy(temp, 0, res, 0, i)
            res

        /// <summary>Returns the first N elements of the array.</summary>
        /// <remarks>Throws <c>InvalidOperationException</c>
        /// if the count exceeds the number of elements in the array. <c>Array.truncate</c>
        /// returns as many items as the array contains instead of throwing an exception.</remarks>
        ///
        /// <param name="count">The number of items to take.</param>
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The result array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        /// <exception cref="System.InvalidOperationException">Thrown when count exceeds the number of elements
        /// in the list.</exception>
        [<CompiledName("Take")>]
        let take count (array : 'T[]) =
            checkNonNull "array" array
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            if count = 0 then empty else
            if count > array.Length then
                raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))

            let res : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count

            Array.Copy(array, 0, res, 0, count)
            res

        /// <summary>Returns an array that contains all elements of the original array while the 
        /// given predicate returns <c>true</c>, and then returns no further elements.</summary>
        ///
        /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The result array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("TakeWhile")>]
        let takeWhile predicate (array: 'T[]) =
            checkNonNull "array" array
            if array.Length = 0 then empty else
            let mutable count = 0
            while count < array.Length && predicate array.[count] do
                count <- count + 1

            let res : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count

            Array.Copy(array, 0, res, 0, count)
            res

        /// <summary>Splits an array into two arrays, at the given index.</summary>
        /// <param name="index">The index at which the array is split.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The two split arrays.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <exception cref="System.InvalidOperationException">Thrown when split index exceeds the number of elements
        /// in the array.</exception>
        [<CompiledName("SplitAt")>]
        let splitAt index (array:'T[]) =
            checkNonNull "array" array
            if index < 0 then invalidArg "index" (SR.GetString(SR.inputMustBeNonNegative))
            if array.Length < index then raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
            if index = 0 then
                let right : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked array.Length
                Array.Copy(array, right, array.Length)
                [||],right
            elif index = array.Length then
                let left : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked array.Length
                Array.Copy(array, left, array.Length)
                left,[||] else

            let res1 : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked index
            let res2 : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (array.Length-index)

            Array.Copy(array, 0, res1, 0, index)
            Array.Copy(array, index, res2, 0, array.Length-index)

            res1,res2

        /// <summary>Creates an array by replicating the given initial value.</summary>
        /// <param name="count">The number of elements to replicate.</param>
        /// <param name="initial">The value to replicate</param>
        /// <returns>The generated array.</returns>
        [<CompiledName("Replicate")>]
        let replicate count x = 
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            let arr = (Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count : 'T array)  
            for i = 0 to count - 1 do 
                arr.[i] <- x
            arr

        /// <summary>Compares two arrays using the given comparison function, element by element.
        /// Returns the first non-zero result from the comparison function.  If the end of an array
        /// is reached it returns a -1 if the first array is shorter and a 1 if the second array
        /// is shorter.</summary>
        ///
        /// <param name="comparer">A function that takes an element from each array and returns an int.
        /// If it evaluates to a non-zero value iteration is stopped and that value is returned.</param>
        /// <param name="array1">The first input array.</param>
        /// <param name="array2">The second input array.</param>
        ///
        /// <returns>The first non-zero value from the comparison function.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when either of the input arrays
        /// is null.</exception>
        [<CompiledName("CompareWith")>]
        let inline compareWith (comparer:'T -> 'T -> int) (array1: 'T[]) (array2: 'T[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2

            let length1 = array1.Length
            let length2 = array2.Length
            let minLength = Operators.min length1 length2

            let rec loop index  =
                if index = minLength  then
                    if length1 = length2 then 0
                    elif length1 < length2 then -1
                    else 1
                else
                    let result = comparer array1.[index] array2.[index]
                    if result <> 0 then result else
                    loop (index+1)

            loop 0

        /// <summary>Applies a key-generating function to each element of an array and returns an array yielding unique
        /// keys and their number of occurrences in the original array.</summary>
        ///
        /// <param name="projection">A function transforming each item of the input array into a key to be
        /// compared against the others.</param>
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The result array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("CountBy")>]
        let countBy projection (array:'T[]) =
            checkNonNull "array" array
            let dict = new Dictionary<Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox<'Key>,int>(Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox<'Key>.Comparer)

            // Build the groupings
            for v in array do
                let key = Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox (projection v)
                let mutable prev = Unchecked.defaultof<_>
                if dict.TryGetValue(key, &prev) then dict.[key] <- prev + 1 else dict.[key] <- 1

            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked dict.Count
            let mutable i = 0
            for group in dict do
                res.[i] <- group.Key.Value, group.Value
                i <- i + 1
            res

        /// <summary>Returns the first element of the array, or
        /// <c>None</c> if the array is empty.</summary>
        /// <param name="array">The input array.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>The first element of the array or None.</returns>
        [<CompiledName("TryHead")>]
        let tryHead (array : 'T[]) =
            checkNonNull "array" array
            if array.Length = 0 then None
            else Some array.[0]

        /// <summary>Returns a new array containing only the elements of the array
        /// for which the given predicate returns "true".</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="array">The input array.</param>
        /// <returns>An array containing the elements for which the given predicate returns true.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>    
        [<CompiledName("Where")>]
        let where f (array: _[]) = filter f array

        /// <summary>Returns the index of the last element in the array
        /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if
        /// none of the elements satisfy the predicate.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="array">The input array.</param>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c>
        /// never returns true.</exception>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>The index of the last element in the array that satisfies the given predicate.</returns>
        [<CompiledName("FindIndexBack")>]
        let findIndexBack f (array : _[]) =
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.findIndexBack f array

        /// <summary>Returns the last element of the array.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>The last element of the array.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>        
        /// <exception cref="System.ArgumentException">Thrown when the input does not have any elements.</exception>
        [<CompiledName("Last")>]
        let inline last (array : 'T[]) =
            checkNonNull "array" array
            if array.Length = 0 then invalidArg "array" InputArrayEmptyString
            array.[array.Length-1]

        /// <summary>Returns the last element of the array.
        /// Return <c>None</c> if no such element exists.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>The last element of the array or None.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception> 
        [<CompiledName("TryLast")>]
        let tryLast (array : 'T[]) =
            checkNonNull "array" array
            if array.Length = 0 then None
            else Some array.[array.Length-1]

        /// <summary>Returns the only element of the array.</summary>
        ///
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The only element of the array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when the input does not have precisely one element.</exception>
        [<CompiledName("ExactlyOne")>]
        let exactlyOne (array:'T[]) =
            checkNonNull "array" array
            if array.Length = 1 then array.[0]
            elif array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
            else invalidArg "array" (SR.GetString(SR.inputSequenceTooLong))

        /// <summary>Applies a key-generating function to each element of an array and yields an array of 
        /// unique keys. Each unique key contains an array of all elements that match 
        /// to this key.</summary>
        ///
        /// <param name="projection">A function that transforms an element of the array into a comparable key.</param>
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The result array.</returns>
        [<CompiledName("GroupBy")>]
        let groupBy keyf (array: 'T[]) =
            checkNonNull "array" array
            let dict = new Dictionary<RuntimeHelpers.StructBox<'Key>,ResizeArray<'T>>(RuntimeHelpers.StructBox<'Key>.Comparer)

            // Build the groupings
            for i = 0 to (array.Length - 1) do
                let v = array.[i]
                let key = RuntimeHelpers.StructBox (keyf v)
                let ok, prev = dict.TryGetValue(key)
                if ok then 
                    prev.Add(v)
                else 
                    let prev = new ResizeArray<'T>(1)
                    dict.[key] <- prev
                    prev.Add(v)
                     
            // Return the array-of-arrays.
            let result = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked dict.Count
            let mutable i = 0
            for group in dict do
                result.[i] <- group.Key.Value, group.Value.ToArray()
                i <- i + 1

            result

        /// <summary>Returns the first element of the array.</summary>
        ///
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The first element of the array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        [<CompiledName("Head")>]
        let head (array : 'T[]) =
            checkNonNull "array" array
            if array.Length = 0 then invalidArg "array" InputArrayEmptyString else array.[0]

        /// <summary>Returns an array of each element in the input array and its predecessor, with the
        /// exception of the first element which is only returned as the predecessor of the second element.</summary>
        ///
        /// <param name="array">The input array.</param>
        ///
        /// <returns>The result array.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("Pairwise")>]
        let pairwise (array: 'T[]) =
            checkNonNull "array" array
            if array.Length < 2 then [||] else
            init (array.Length-1) (fun i -> array.[i],array.[i+1])

        /// <summary>Returns an array that contains one item only.</summary>
        ///
        /// <param name="value">The input item.</param>
        ///
        /// <returns>The result array of one item.</returns>
        [<CompiledName("Singleton")>]
        let inline singleton value = [|value|]

        /// <summary>Tests if the array contains the specified element.</summary>
        /// <param name="value">The value to locate in the input array.</param>
        /// <param name="array">The input array.</param>
        /// <returns>True if the input array contains the specified element; false otherwise.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("Contains")>]
        let inline contains e (array:'T[]) =
            checkNonNull "array" array
            let mutable state = false
            let mutable i = 0
            while (not state && i < array.Length) do
                state <- e = array.[i]
                i <- i + 1
            state

        /// <summary>Builds a new array whose elements are the corresponding elements of the input array
        /// paired with the integer index (from 0) of each element.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>The array of indexed elements.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("Indexed")>]
        let indexed (array: 'T[]) =
            checkNonNull "array" array
            let len = array.Length
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len
            for i = 0 to len - 1 do
                res.[i] <- (i,array.[i])
            res

        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to the corresponding triples from the three collections. The three input
        /// arrays must have the same length, otherwise an <c>ArgumentException</c> is
        /// raised.</summary>
        /// <param name="mapping">The function to transform the pairs of the input elements.</param>
        /// <param name="array1">The first input array.</param>
        /// <param name="array2">The second input array.</param>
        /// <param name="array3">The third input array.</param>
        /// <exception cref="System.ArgumentException">Thrown when the input arrays differ in length.</exception>
        /// <exception cref="System.ArgumentNullException">Thrown when any of the input arrays is null.</exception>
        /// <returns>The array of transformed elements.</returns>
        [<CompiledName("Map3")>]
        let map3 f (array1: 'T1[]) (array2: 'T2[]) (array3: 'T3[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            checkNonNull "array3" array3
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let len1 = array1.Length
            if not (len1 = array2.Length && len1 = array3.Length) then invalidArg "" (SR.GetString(SR.arraysHadDifferentLengths))

            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len1
            for i = 0 to len1 - 1 do
                res.[i] <- f.Invoke(array1.[i], array2.[i], array3.[i])
            res

        /// <summary>Combines map and fold. Builds a new array whose elements are the results of applying the given function
        /// to each of the elements of the input array. The function is also used to accumulate a final value.</summary>
        /// <param name="mapping">The function to transform elements from the input array and accumulate the final value.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="array">The input array.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>The array of transformed elements, and the final accumulated value.</returns>
        [<CompiledName("MapFold")>]
        let mapFold<'T,'State,'Result> (f : 'State -> 'T -> 'Result * 'State) acc array =
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.mapFold f acc array

        /// <summary>Combines map and foldBack. Builds a new array whose elements are the results of applying the given function
        /// to each of the elements of the input array. The function is also used to accumulate a final value.</summary>
        /// <param name="mapping">The function to transform elements from the input array and accumulate the final value.</param>
        /// <param name="array">The input array.</param>
        /// <param name="state">The initial state.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>The array of transformed elements, and the final accumulated value.</returns>
        [<CompiledName("MapFoldBack")>]
        let mapFoldBack<'T,'State,'Result> (f : 'T -> 'State -> 'Result * 'State) array acc =
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.mapFoldBack f array acc

        /// <summary>Builds a new array that contains the elements of the given array, excluding the first N elements.</summary>
        /// <param name="count">The number of elements to skip.</param>
        /// <param name="array">The input array.</param>
        /// <returns>A copy of the input array, after removing the first N elements.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <exception cref="System.ArgumentExcepion">Thrown when count is negative or exceeds the number of 
        /// elements in the array.</exception>
        [<CompiledName("Skip")>]
        let skip count (array:'T[]) =
            checkNonNull "array" array
            if count > array.Length then invalidArg "count" (SR.GetString(SR.outOfRange))
            if count = array.Length then
                [| |]
            else
                let count = max count 0
                let skippedLen = array.Length - count
                let res : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked skippedLen
                Array.Copy(array, count, res, 0, skippedLen)
                res

        /// <summary>Bypasses elements in an array while the given predicate returns <c>true</c>, and then returns
        /// the remaining elements in a new array.</summary>
        /// <param name="predicate">A function that evaluates an element of the array to a boolean value.</param>
        /// <param name="source">The input array.</param>
        /// <returns>The created sub array.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("SkipWhile")>]
        let skipWhile p (array: 'T[]) =
            checkNonNull "array" array
            let mutable i = 0
            let len = array.Length
            while i < len && p array.[i] do i <- i + 1

            match len - i with
            | 0 -> [| |]
            | resLen ->
                let res : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked resLen
                Array.Copy(array, i, res, 0, resLen)
                res

        /// <summary>Sorts the elements of an array, in descending order, returning a new array. Elements are compared using Operators.compare. </summary>
        ///
        /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
        /// For a stable sort, consider using Seq.sort.</remarks>
        /// <param name="array">The input array.</param>
        /// <returns>The sorted array.</returns>
        [<CompiledName("SortDescending")>]
        let inline sortDescending array =
            checkNonNull "array" array
            let inline compareDescending a b = compare b a
            sortWith compareDescending array

        /// <summary>Sorts the elements of an array, in descending order, using the given projection for the keys and returning a new array. 
        /// Elements are compared using Operators.compare.</summary>
        ///
        /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
        /// For a stable sort, consider using Seq.sort.</remarks>
        /// <param name="projection">The function to transform array elements into the type that is compared.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The sorted array.</returns>
        [<CompiledName("SortByDescending")>]
        let inline sortByDescending f array =
            checkNonNull "array" array
            let inline compareDescending a b = compare (f b) (f a)
            sortWith compareDescending array

        /// <summary>Returns a new array containing the elements of the original except the first element.</summary>
        ///
        /// <param name="array">The input array.</param>
        /// <exception cref="System.ArgumentException">Thrown when the array is empty.</exception>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>A new array containing the elements of the original except the first element.</returns>
        [<CompiledName("Tail")>]
        let tail (array : 'T[]) =
            checkNonNull "array" array
            if array.Length = 0 then invalidArg "array" (SR.GetString(SR.notEnoughElements))
            let len = array.Length - 1
            let result : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len
            Array.Copy(array, 1, result, 0, len)
            result

        /// <summary>Returns at most N elements in a new array.</summary>
        /// <param name="count">The maximum number of items to return.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The result array.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when the count is negative.</exception>
        [<CompiledName("Truncate")>]
        let truncate count (array:'T[]) =
            checkNonNull "array" array
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            if count = 0 then empty
            else
                let len = array.Length
                let count' = Operators.min count len
                let result : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count'
                Array.Copy(array, result, count')
                result

        /// <summary>Returns the index of the last element in the array
        /// that satisfies the given predicate.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="array">The input array.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <returns>The index of the last element that satisfies the predicate, or None.</returns>
        [<CompiledName("TryFindIndexBack")>]
        let tryFindIndexBack f (array : _[]) =
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.tryFindIndexBack f array

        /// <summary>Returns an array that contains the elements generated by the given computation.
        /// The given initial <c>state</c> argument is passed to the element generator.</summary>
        /// <param name="generator">A function that takes in the current state and returns an option tuple of the next
        /// element of the array and the next state value.</param>
        /// <param name="state">The initial state value.</param>
        /// <returns>The result array.</returns>
        [<CompiledName("Unfold")>]
        let unfold<'T,'State> (f:'State -> ('T*'State) option) (s:'State) =
            let res = ResizeArray<_>()
            let rec loop state =
                match f state with
                | None -> ()
                | Some (x,s') ->
                    res.Add(x)
                    loop s'
            loop s
            res.ToArray()

        /// <summary>Returns an array of sliding windows containing elements drawn from the input
        /// array. Each window is returned as a fresh array.</summary>
        /// <param name="windowSize">The number of elements in each window.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The result array.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when windowSize is not positive.</exception>
        [<CompiledName("Windowed")>]
        let windowed windowSize (array:'T[]) =
            checkNonNull "array" array
            if windowSize <= 0 then invalidArg "windowSize" (SR.GetString(SR.inputMustBeNonNegative))
            let len = array.Length
            if windowSize > len then
                [| |]
            else
                let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (len - windowSize + 1) : 'T[][]
                for i = 0 to len - windowSize do
                    res.[i] <- array.[i..i+windowSize-1]
                res

        /// <summary>Gets an element from an array.</summary>
        /// <param name="index">The input index.</param>
        /// <param name="array">The input array.</param>
        /// <returns>The value of the array at the given index.</returns>
        /// <exception cref="System.NullReferenceException">Thrown when the input array is null.</exception>
        /// <exception cref="System.IndexOutOfRangeException">Thrown when the index is negative or the input array does not contain enough elements.</exception>
        [<CompiledName("Item")>]
        let item n (array:_[]) =
            array.[n]

        /// <summary>Tries to find the nth element in the array.
        /// Returns <c>None</c> if index is negative or the input array does not contain enough elements.</summary>
        /// <param name="index">The index of element to retrieve.</param>
        /// <param name="source">The input array.</param>
        /// <returns>The nth element of the array or <c>None</c>.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
        [<CompiledName("TryItem")>]
        let tryItem index (array:'T[]) =
            checkNonNull "array" array
            if index < 0 || index >= array.Length then None
            else Some(array.[index])

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module List =
        open System.Collections.Generic
        open Microsoft.FSharp.Core
        open Local

        let private rev = List.rev
        let private toArray = List.toArray
        let sortWith = List.sortWith

        /// <summary>Returns a list that contains no duplicate entries according to generic hash and
        /// equality comparisons on the entries.
        /// If an element occurs multiple times in the list then the later occurrences are discarded.</summary>
        ///
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        [<CompiledName("Distinct")>]
        let distinct (list:'T list) = Microsoft.FSharp.Primitives.Basics.List.distinctWithComparer HashIdentity.Structural<'T> list

        /// <summary>Returns a list that contains no duplicate entries according to the 
        /// generic hash and equality comparisons on the keys returned by the given key-generating function.
        /// If an element occurs multiple times in the list then the later occurrences are discarded.</summary>
        ///
        /// <param name="projection">A function transforming the list items into comparable keys.</param>
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        [<CompiledName("DistinctBy")>]
        let distinctBy keyf (list:'T list) = Microsoft.FSharp.Primitives.Basics.List.distinctByWithComparer HashIdentity.Structural<_> keyf list

        /// <summary>Returns the first N elements of the list.</summary>
        /// <remarks>Throws <c>InvalidOperationException</c>
        /// if the count exceeds the number of elements in the list. <c>List.truncate</c>
        /// returns as many items as the list contains instead of throwing an exception.</remarks>
        ///
        /// <param name="count">The number of items to take.</param>
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        ///
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <exception cref="System.InvalidOperationException">Thrown when count exceeds the number of elements
        /// in the list.</exception>
        [<CompiledName("Take")>]
        let take count (list : 'T list) = Microsoft.FSharp.Primitives.Basics.List.take count list

        /// <summary>Compares two lists using the given comparison function, element by element.
        /// Returns the first non-zero result from the comparison function.  If the end of a list
        /// is reached it returns a -1 if the first list is shorter and a 1 if the second list
        /// is shorter.</summary>
        ///
        /// <param name="comparer">A function that takes an element from each list and returns an int.
        /// If it evaluates to a non-zero value iteration is stopped and that value is returned.</param>
        /// <param name="list1">The first input list.</param>
        /// <param name="list2">The second input list.</param>
        ///
        /// <returns>The first non-zero value from the comparison function.</returns>
        [<CompiledName("CompareWith")>]
        let inline compareWith (comparer:'T -> 'T -> int) (list1: 'T list) (list2: 'T list) =
            let rec loop list1 list2 =
                 match list1, list2 with
                 | head1 :: tail1, head2 :: tail2 ->
                       let c = comparer head1 head2
                       if c = 0 then loop tail1 tail2 else c
                 | [], [] -> 0
                 | _, [] -> 1
                 | [], _ -> -1

            loop list1 list2

        /// <summary>Returns a list that contains all elements of the original list while the 
        /// given predicate returns <c>true</c>, and then returns no further elements.</summary>
        ///
        /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        [<CompiledName("TakeWhile")>]
        let takeWhile p (list: 'T list) = Microsoft.FSharp.Primitives.Basics.List.takeWhile p list

        /// <summary>Splits a list into two lists, at the given index.</summary>
        /// <param name="index">The index at which the list is split.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The two split lists.</returns>
        ///
        /// <exception cref="System.InvalidOperationException">Thrown when split index exceeds the number of elements
        /// in the list.</exception>
        [<CompiledName("SplitAt")>]
        let splitAt index (list:'T list) = Microsoft.FSharp.Primitives.Basics.List.splitAt index list

        /// <summary>Applies a key-generating function to each element of a list and returns a list yielding unique
        /// keys and their number of occurrences in the original list.</summary>
        ///
        /// <param name="projection">A function transforming each item of the input list into a key to be
        /// compared against the others.</param>
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        [<CompiledName("CountBy")>]
        let countBy projection (list:'T list) =
            let dict = new Dictionary<Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox<'Key>,int>(Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox<'Key>.Comparer)
            let rec loop srcList  =
                match srcList with
                | [] -> ()
                | h::t ->
                    let key = Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox (projection h)
                    let mutable prev = 0
                    if dict.TryGetValue(key, &prev) then dict.[key] <- prev + 1 else dict.[key] <- 1
                    loop t
            loop list
            let mutable result = []
            for group in dict do
                result <- (group.Key.Value, group.Value) :: result
            result |> rev

        /// <summary>Returns a new list containing only the elements of the list
        /// for which the given predicate returns "true"</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="list">The input list.</param>
        /// <returns>A list containing only the elements that satisfy the predicate.</returns>
        [<CompiledName("Where")>]
        let where f x = Microsoft.FSharp.Primitives.Basics.List.filter f x

        /// <summary>Returns the last element for which the given function returns <c>true</c>.
        /// Raises <c>KeyNotFoundException</c> if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="list">The input list.</param>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if the predicate evaluates to false for
        /// all the elements of the list.</exception>
        /// <returns>The last element that satisfies the predicate.</returns>
        [<CompiledName("FindBack")>]
        let findBack f list = list |> toArray |> Array.findBack f

        /// <summary>Returns the index of the last element in the list
        /// that satisfies the given predicate.
        /// Raises <c>KeyNotFoundException</c> if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="list">The input list.</param>
        /// <exception cref="System.ArgumentException">Thrown if the predicate evaluates to false for all the
        /// elements of the list.</exception>
        /// <returns>The index of the last element that satisfies the predicate.</returns>
        [<CompiledName("FindIndexBack")>]
        let findIndexBack f list = list |> toArray |> Array.findIndexBack f

        /// <summary>Applies a key-generating function to each element of a list and yields a list of 
        /// unique keys. Each unique key contains a list of all elements that match 
        /// to this key.</summary>
        ///
        /// <param name="projection">A function that transforms an element of the list into a comparable key.</param>
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        [<CompiledName("GroupBy")>]
        let groupBy keyf (list: 'T list) =
            let dict = new Dictionary<Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox<'Key>,ResizeArray<'T>>(Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox<'Key>.Comparer)

            // Build the groupings
            let rec loop list =
                match list with
                | v :: t -> 
                    let key = Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.StructBox (keyf v)
                    let ok,prev = dict.TryGetValue(key)
                    if ok then
                        prev.Add(v)
                    else 
                        let prev = new ResizeArray<'T>(1)
                        dict.[key] <- prev
                        prev.Add(v)
                    loop t
                | _ -> ()
            loop list

            // Return the list-of-lists.
            dict
            |> Seq.map (fun group -> (group.Key.Value, Seq.toList group.Value))
            |> Seq.toList

        /// <summary>Returns the only element of the list.</summary>
        ///
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The only element of the list.</returns>
        ///        
        /// <exception cref="System.ArgumentException">Thrown when the input does not have precisely one element.</exception>
        [<CompiledName("ExactlyOne")>]
        let exactlyOne (source : list<_>) =
            match source with
            | [x] -> x
            | []  -> invalidArg "source" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
            | _   -> invalidArg "source" (SR.GetString(SR.inputSequenceTooLong))

        /// <summary>Returns the first element of the list, or
        /// <c>None</c> if the list is empty.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>The first element of the list or None.</returns>
        [<CompiledName("TryHead")>]
        let tryHead list = match list with (x:: _) -> Some x | [] -> None

        /// <summary>Returns the last element of the list.
        /// Return <c>None</c> if no such element exists.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>The last element of the list or None.</returns>
        [<CompiledName("TryLast")>]
        let rec tryLast (list: 'T list) =
            match list with
            | [x] -> Some x
            | _ :: tail -> tryLast tail
            | [] -> None

        /// <summary>Returns the last element of the list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>The last element of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input does not have any elements.</exception>
        [<CompiledName("Last")>]
        let rec last (list : 'T list) =
            match list with
            | [x] -> x
            | _ :: tail -> last tail
            | [] -> invalidArg "list" (SR.GetString(SR.inputListWasEmpty))

        /// <summary>Tests if the list contains the specified element.</summary>
        /// <param name="value">The value to locate in the input list.</param>
        /// <param name="source">The input list.</param>
        /// <returns>True if the input list contains the specified element; false otherwise.</returns>
        [<CompiledName("Contains")>]
        let inline contains e list1 =
            let rec contains e xs1 =
                match xs1 with
                | [] -> false
                | (h1::t1) -> e = h1 || contains e t1
            contains e list1

        /// <summary>Returns a list that contains one item only.</summary>
        ///
        /// <param name="value">The input item.</param>
        ///
        /// <returns>The result list of one item.</returns>
        [<CompiledName("Singleton")>]
        let inline singleton value = [value]

        /// <summary>Returns a list of each element in the input list and its predecessor, with the
        /// exception of the first element which is only returned as the predecessor of the second element.</summary>
        ///
        /// <param name="list">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        [<CompiledName("Pairwise")>]
        let pairwise (list: 'T list) =
            let array = List.toArray list
            if array.Length < 2 then [] else
            List.init (array.Length-1) (fun i -> array.[i],array.[i+1])

        /// <summary>Returns a new list whose elements are the corresponding elements
        /// of the input list paired with the index (from 0) of each element.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>The list of indexed elements.</returns>
        [<CompiledName("Indexed")>]
        let indexed list = Microsoft.FSharp.Primitives.Basics.List.indexed list

        /// <summary>Combines map and fold. Builds a new list whose elements are the results of applying the given function
        /// to each of the elements of the input list. The function is also used to accumulate a final value.</summary>
        /// <param name="mapping">The function to transform elements from the input list and accumulate the final value.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The list of transformed elements, and the final accumulated value.</returns>
        [<CompiledName("MapFold")>]
        let mapFold<'T,'State,'Result> (f:'State -> 'T -> 'Result * 'State) acc list =
            Microsoft.FSharp.Primitives.Basics.List.mapFold f acc list

        /// <summary>Combines map and foldBack. Builds a new list whose elements are the results of applying the given function
        /// to each of the elements of the input list. The function is also used to accumulate a final value.</summary>
        /// <param name="mapping">The function to transform elements from the input list and accumulate the final value.</param>
        /// <param name="list">The input list.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The list of transformed elements, and the final accumulated value.</returns>
        [<CompiledName("MapFoldBack")>]
        let mapFoldBack<'T,'State,'Result> (f:'T -> 'State -> 'Result * 'State) list acc =
            match list with
            | [] -> [], acc
            | [h] -> let h',s' = f h acc in [h'], s'
            | _ ->
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let rec loop res list =
                    match list, res with
                    | [], _ -> res
                    | h::t, (list', acc') ->
                        let h',s' = f.Invoke(h,acc')
                        loop (h'::list', s') t
                loop ([], acc) (rev list)

        /// <summary>Indexes into the list. The first element has index 0.</summary>
        /// <param name="index">The index to retrieve.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The value at the given index.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the index is negative or the input list does not contain enough elements.</exception>
        [<CompiledName("Item")>]
        let rec item index list =
            match list with
            | h::t when index >= 0 ->
                if index = 0 then h else item (index - 1) t
            | _ ->
                invalidArg "index" (SR.GetString(SR.indexOutOfBounds))

        /// <summary>Returns the list after removing the first N elements.</summary>
        /// <param name="count">The number of elements to skip.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The list after removing the first N elements.</returns>
        /// <exception cref="System.ArgumentException">Thrown when count is negative or exceeds the number of 
        /// elements in the list.</exception>
        [<CompiledName("Skip")>]
        let skip count list =
            if count <= 0 then list else
            let rec loop i lst =
                match lst with
                | _ when i = 0 -> lst
                | _::t -> loop (i-1) t
                | [] -> invalidArg "count" (SR.GetString(SR.outOfRange))
            loop count list

        /// <summary>Bypasses elements in a list while the given predicate returns <c>true</c>, and then returns
        /// the remaining elements of the list.</summary>
        /// <param name="predicate">A function that evaluates an element of the list to a boolean value.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The result list.</returns>
        [<CompiledName("SkipWhile")>]
        let rec skipWhile p xs =
            match xs with
            | head :: tail when p head -> skipWhile p tail
            | _ -> xs

        /// <summary>Sorts the given list in descending order using Operators.compare.</summary>
        ///
        /// <remarks>This is a stable sort, i.e. the original order of equal elements is preserved.</remarks>
        /// <param name="list">The input list.</param>
        /// <returns>The sorted list.</returns>
        [<CompiledName("SortDescending")>]
        let inline sortDescending xs =
            let inline compareDescending a b = compare b a
            sortWith compareDescending xs

        /// <summary>Sorts the given list in descending order using keys given by the given projection. Keys are compared using Operators.compare.</summary>
        ///
        /// <remarks>This is a stable sort, i.e. the original order of equal elements is preserved.</remarks>
        /// <param name="projection">The function to transform the list elements into the type to be compared.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The sorted list.</returns>
        [<CompiledName("SortByDescending")>]
        let inline sortByDescending f xs =
            let inline compareDescending a b = compare (f b) (f a)
            sortWith compareDescending xs

        /// <summary>Returns at most N elements in a new list.</summary>
        /// <param name="count">The maximum number of items to return.</param>
        /// <param name="array">The input list.</param>
        /// <returns>The result list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the count is negative.</exception>
        [<CompiledName("Truncate")>]
        let truncate count list = Microsoft.FSharp.Primitives.Basics.List.truncate count list

        /// <summary>Returns the last element for which the given function returns <c>true.</c>.
        /// Return <c>None</c> if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The last element for which the predicate returns true, or None if
        /// every element evaluates to false.</returns>
        [<CompiledName("TryFindBack")>]
        let tryFindBack f list = list |> toArray |> Array.tryFindBack f

        /// <summary>Returns the index of the last element in the list
        /// that satisfies the given predicate.
        /// Return <c>None</c> if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The index of the last element for which the predicate returns true, or None if
        /// every element evaluates to false.</returns>
        [<CompiledName("TryFindIndexBack")>]
        let tryFindIndexBack f list = list |> toArray |> Array.tryFindIndexBack f

        /// <summary>Returns a list that contains the elements generated by the given computation.
        /// The given initial <c>state</c> argument is passed to the element generator.</summary>
        /// <param name="generator">A function that takes in the current state and returns an option tuple of the next
        /// element of the list and the next state value.</param>
        /// <param name="state">The initial state value.</param>
        /// <returns>The result list.</returns>
        [<CompiledName("Unfold")>]
        let unfold<'T,'State> (f:'State -> ('T*'State) option) (s:'State) = Microsoft.FSharp.Primitives.Basics.List.unfold f s

        /// <summary>Returns a list of sliding windows containing elements drawn from the input
        /// list. Each window is returned as a fresh array.</summary>
        /// <param name="windowSize">The number of elements in each window.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The result list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when windowSize is not positive.</exception>
        [<CompiledName("Windowed")>]
        let windowed n x = Microsoft.FSharp.Primitives.Basics.List.windowed n x

        /// <summary>Tries to find the nth element in the list.
        /// Returns <c>None</c> if index is negative or the list does not contain enough elements.</summary>
        /// <param name="index">The index to retrieve.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The value at the given index or <c>None</c>.</returns>
        [<CompiledName("TryItem")>]
        let rec tryItem index list =
            match list with
            | h::t when index >= 0 ->
                if index = 0 then Some h else tryItem (index - 1) t
            | _ ->
                None

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Seq =
        open System
        open System.Collections
        open System.Collections.Generic
        open Microsoft.FSharp.Core
        open Microsoft.FSharp.Primitives.Basics
        open Local.LanguagePrimitives.ErrorStrings

        let private toArray = Seq.toArray
        let private readonly = Seq.readonly
        let private mapi = Seq.mapi

        let private mkSeq f =
            { new IEnumerable<'U> with
                member x.GetEnumerator() = f()
              interface IEnumerable with
                member x.GetEnumerator() = (f() :> IEnumerator) }

        let private mkDelayedSeq (f: unit -> IEnumerable<'T>) = mkSeq (fun () -> f().GetEnumerator())

        let private foldArraySubRight (f:OptimizedClosures.FSharpFunc<'T,_,_>) (arr: 'T[]) start fin acc =
            let mutable state = acc
            for i = fin downto start do
                state <- f.Invoke(arr.[i], state)
            state

        let private revamp2 f (ie1 : seq<_>) (source2 : seq<_>) =
            mkSeq (fun () -> f (ie1.GetEnumerator()) (source2.GetEnumerator()))
        let private revamp3 f (ie1 : seq<_>) (source2 : seq<_>) (source3 : seq<_>) =
            mkSeq (fun () -> f (ie1.GetEnumerator()) (source2.GetEnumerator()) (source3.GetEnumerator()))

        let inline private checkNonNull argName arg =
            match box arg with 
            | null -> nullArg argName
            | _ -> ()

        /// <summary>Creates a sequence by replicating the given initial value.</summary>
        /// <param name="count">The number of elements to replicate.</param>
        /// <param name="initial">The value to replicate</param>
        /// <returns>The generated sequence.</returns>
        [<CompiledName("Replicate")>]
        let replicate count x =
            System.Linq.Enumerable.Repeat(x,count)

        /// <summary>Returns the last element for which the given function returns <c>true</c>.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a
        /// result this function should not be used with large or infinite sequences.</remarks>
        /// <param name="predicate">A function to test whether an item in the sequence should be returned.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The last element for which the predicate returns <c>true</c>.</returns>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if no element returns true when
        /// evaluated by the predicate</exception>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null</exception>
        [<CompiledName("FindBack")>]
        let findBack f source =
            checkNonNull "source" source
            source |> Seq.toArray |> Array.findBack f

        /// <summary>Returns a sequence with all elements permuted according to the
        /// specified permutation.</summary>
        ///
        /// <param name="indexMap">The function that maps input indices to output indices.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("Permute")>]
        let permute f (source : seq<_>) =
            checkNonNull "source" source
            mkDelayedSeq (fun () ->
                source |> toArray |> Array.permute f :> seq<_>)

        /// <summary>Returns the index of the last element in the sequence
        /// that satisfies the given predicate. Return <c>None</c> if no such element exists.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a
        /// result this function should not be used with large or infinite sequences.</remarks>
        /// <param name="predicate">A function that evaluates to a Boolean when given an item in the sequence.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The found index or <c>None</c>.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("TryFindIndexBack")>]
        let tryFindIndexBack f (source : seq<'T>) =
            checkNonNull "source" source
            source |> toArray |> Array.tryFindIndexBack f

        /// <summary>Returns the index of the last element for which the given function returns <c>true</c>.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a
        /// result this function should not be used with large or infinite sequences.</remarks>
        /// <param name="predicate">A function to test whether the index of a particular element should be returned.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The index of the last element for which the predicate returns <c>true</c>.</returns>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if no element returns true when
        /// evaluated by the predicate</exception>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null</exception>
        [<CompiledName("FindIndexBack")>]
        let findIndexBack f source =
            checkNonNull "source" source
            source |> toArray |> Array.findIndexBack f

        /// <summary>Applies a function to corresponding elements of two collections, threading an accumulator argument
        /// through the computation. The two sequences need not have equal lengths:
        /// when one sequence is exhausted any remaining elements in the other sequence are ignored.
        /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c>
        /// then computes <c>f (... (f s i0 j0)...) iN jN</c>.</summary>
        /// <param name="folder">The function to update the state given the input elements.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        /// <returns>The final state value.</returns>
        [<CompiledName("Fold2")>]
        let fold2<'T1,'T2,'State> f (state:'State) (source1: seq<'T1>) (source2: seq<'T2>) =
            checkNonNull "source1" source1
            checkNonNull "source2" source2

            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()

            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)

            let mutable state = state
            while e1.MoveNext() && e2.MoveNext() do
                state <- f.Invoke(state, e1.Current, e2.Current)

            state

        /// <summary>Applies a function to each element of the collection, starting from the end, threading an accumulator argument
        /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
        /// then computes <c>f i0 (... (f iN s)...)</c></summary>
        /// <param name="folder">The function to update the state given the input elements.</param>
        /// <param name="source">The input sequence.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The state object after the folding function is applied to each element of the sequence.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("FoldBack")>]
        let foldBack<'T,'State> f (source : seq<'T>) (x:'State) =
            checkNonNull "source" source
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let arr = toArray source
            let len = arr.Length
            foldArraySubRight f arr 0 (len - 1) x

        /// <summary>Tests if the sequence contains the specified element.</summary>
        /// <param name="value">The value to locate in the input sequence.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>True if the input sequence contains the specified element; false otherwise.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("Contains")>]
        let inline contains element (source : seq<'T>) =
            checkNonNull "source" source
            use e = source.GetEnumerator()
            let mutable state = false
            while (not state && e.MoveNext()) do
                state <- element = e.Current
            state

        /// <summary>Returns the first element of the sequence, or None if the sequence is empty.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The first element of the sequence or None.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("TryHead")>]
        let tryHead (source : seq<_>) =
            checkNonNull "source" source
            use e = source.GetEnumerator()
            if (e.MoveNext()) then Some e.Current
            else None

        /// <summary>Returns a sequence that skips 1 element of the underlying sequence and then yields the
        /// remaining elements of the sequence.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        /// <exception cref="System.InvalidOperationException">Thrown when the input sequence is empty.</exception>
        [<CompiledName("Tail")>]
        let tail (source: seq<'T>) =
            checkNonNull "source" source
            seq { use e = source.GetEnumerator()
                  if not (e.MoveNext()) then
                      invalidArg "source" (SR.GetString(SR.notEnoughElements))
                  while e.MoveNext() do
                      yield e.Current }

        /// <summary>Returns the last element of the sequence.
        /// Return <c>None</c> if no such element exists.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The last element of the sequence or None.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("TryLast")>]
        let tryLast (source : seq<_>) =
            checkNonNull "source" source
            use e = source.GetEnumerator()
            if e.MoveNext() then
                let mutable res = e.Current
                while (e.MoveNext()) do res <- e.Current
                Some res
            else
                None

        /// <summary>Applies the given function to two collections simultaneously. If one sequence is shorter than 
        /// the other then the remaining elements of the longer sequence are ignored. The integer passed to the
        /// function indicates the index of element.</summary>
        ///
        /// <param name="action">A function to apply to each pair of elements from the input sequences along with their index.</param>
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when either of the input sequences is null.</exception>
        [<CompiledName("IterateIndexed2")>]
        let iteri2 f (source1 : seq<_>) (source2 : seq<_>) = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let mutable i = 0 
            while (e1.MoveNext() && e2.MoveNext()) do
                f.Invoke(i, e1.Current, e2.Current)
                i <- i + 1

        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to the corresponding triples of elements from the three sequences. If one input sequence if shorter than
        /// the others then the remaining elements of the longer sequences are ignored.</summary>
        ///
        /// <param name="mapping">The function to transform triples of elements from the input sequences.</param>
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        /// <param name="source3">The third input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when any of the input sequences is null.</exception>
        [<CompiledName("Map3")>]
        let map3 f source1 source2 source3 = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            checkNonNull "source3" source3
            revamp3 (IEnumerator.map3    f) source1 source2 source3

        /// <summary>Combines map and fold. Builds a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection. The function is also used to accumulate a final value.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a result this function should
        /// not be used with large or infinite sequences.</remarks>
        /// <param name="mapping">The function to transform elements from the input collection and accumulate the final value.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="array">The input collection.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        /// <returns>The collection of transformed elements, and the final accumulated value.</returns>
        [<CompiledName("MapFold")>]
        let mapFold<'T,'State,'Result> (f: 'State -> 'T -> 'Result * 'State) acc source =
            checkNonNull "source" source
            let arr,state = source |> toArray |> Array.mapFold f acc
            readonly arr, state

        /// <summary>Combines map and foldBack. Builds a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection. The function is also used to accumulate a final value.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a result this function should
        /// not be used with large or infinite sequences.</remarks>
        /// <param name="mapping">The function to transform elements from the input collection and accumulate the final value.</param>
        /// <param name="array">The input collection.</param>
        /// <param name="state">The initial state.</param>
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        /// <returns>The collection of transformed elements, and the final accumulated value.</returns>
        [<CompiledName("MapFoldBack")>]
        let mapFoldBack<'T,'State,'Result> (f: 'T -> 'State -> 'Result * 'State) source acc =
            checkNonNull "source" source
            let array = source |> toArray
            let arr,state = Array.mapFoldBack f array acc
            readonly arr, state

        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to the corresponding pairs of elements from the two sequences. If one input sequence is shorter than 
        /// the other then the remaining elements of the longer sequence are ignored. The integer index passed to the
        /// function indicates the index (from 0) of element being transformed.</summary>
        ///
        /// <param name="mapping">A function to transform pairs of items from the input sequences that also supplies the current index.</param>
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when either of the input sequences is null.</exception>
        [<CompiledName("MapIndexed2")>]
        let mapi2 f source1 source2 =
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            revamp2 (IEnumerator.mapi2    f) source1 source2

        /// <summary>Builds a new collection whose elements are the corresponding elements of the input collection
        /// paired with the integer index (from 0) of each element.</summary>
        /// <param name="source">The input sequence.</param>
        /// <returns>The result sequence.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("Indexed")>]
        let indexed source =
            checkNonNull "source" source
            mapi (fun i x -> i,x) source

        /// <summary>Computes the element at the specified index in the collection.</summary>
        /// <param name="index">The index of the element to retrieve.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The element at the specified index of the sequence.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when the index is negative or the input sequence does not contain enough elements.</exception>
        [<CompiledName("Item")>]
        let item i (source : seq<'T>) =
            checkNonNull "source" source
            if i < 0 then invalidArg "index" (SR.GetString(SR.inputMustBeNonNegative))
            use e = source.GetEnumerator()
            IEnumerator.nth i e

        /// <summary>Applies a function to each element of the sequence, starting from the end, threading an accumulator argument
        /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
        /// then computes <c>f i0 (...(f iN-1 iN))</c>.</summary>
        /// <param name="reduction">A function that takes in the next-to-last element of the sequence and the
        /// current accumulated result to produce the next accumulated result.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The final result of the reductions.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
        [<CompiledName("ReduceBack")>]
        let reduceBack f (source : seq<'T>) =
            checkNonNull "source" source
            let arr = toArray source
            match arr.Length with
            | 0 -> invalidArg "source" InputSequenceEmptyString
            | len ->
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                foldArraySubRight f arr 0 (len - 2) arr.[len - 1]

        /// <summary>Returns a new sequence with the elements in reverse order.</summary>
        /// <param name="source">The input sequence.</param>
        /// <returns>The reversed sequence.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("Reverse")>]
        let rev source =
            checkNonNull "source" source
            mkDelayedSeq (fun () ->
                let array = source |> toArray
                Array.Reverse array
                array :> seq<_>)

        /// <summary>Like <c>foldBack</c>, but returns the sequence of intermediary and final results.</summary>
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as that
        /// sequence is iterated. As a result this function should not be used with large or infinite sequences.
        /// </remarks>
        /// <param name="folder">A function that updates the state with each element from the sequence.</param>
        /// <param name="source">The input sequence.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The resulting sequence of computed states.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("ScanBack")>]
        let scanBack<'T,'State> f (source : seq<'T>) (acc:'State) =
            checkNonNull "source" source
            mkDelayedSeq(fun () ->
                let arr = source |> toArray
                let res = Array.scanSubRight f arr 0 (arr.Length - 1) acc
                res :> seq<_>)

        let sortWith f source =
            checkNonNull "source" source
            mkDelayedSeq (fun () ->
                let array = source |> toArray
                Array.stableSortInPlaceWith f array
                array :> seq<_>)

        /// <summary>Yields a sequence ordered descending by keys.</summary>
        /// 
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
        /// that sequence is iterated. As a result this function should not be used with 
        /// large or infinite sequences. The function makes no assumption on the ordering of the original 
        /// sequence.
        ///
        /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("SortDescending")>]
        let inline sortDescending source =
            checkNonNull "source" source
            let inline compareDescending a b = compare b a
            sortWith compareDescending source

        /// <summary>Applies a key-generating function to each element of a sequence and yield a sequence ordered
        /// descending by keys.  The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary> 
        /// 
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
        /// that sequence is iterated. As a result this function should not be used with 
        /// large or infinite sequences. The function makes no assumption on the ordering of the original 
        /// sequence.
        ///
        /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
        ///
        /// <param name="projection">A function to transform items of the input sequence into comparable keys.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("SortByDescending")>]
        let inline sortByDescending keyf source =
            checkNonNull "source" source
            let inline compareDescending a b = compare (keyf b) (keyf a)
            sortWith compareDescending source

        /// <summary>Returns the last element for which the given function returns <c>true</c>.
        /// Return <c>None</c> if no such element exists.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a
        /// result this function should not be used with large or infinite sequences.</remarks>
        /// <param name="predicate">A function that evaluates to a Boolean when given an item in the sequence.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The found element or <c>None</c>.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("TryFindBack")>]
        let tryFindBack f (source : seq<'T>) =
            checkNonNull "source" source
            source |> toArray |> Array.tryFindBack f

        /// <summary>Tries to find the nth element in the sequence.
        /// Returns <c>None</c> if index is negative or the input sequence does not contain enough elements.</summary>
        /// <param name="index">The index of element to retrieve.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The nth element of the sequence or <c>None</c>.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        [<CompiledName("TryItem")>]
        let tryItem i (source : seq<'T>) =
            checkNonNull "source" source
            if i < 0 then None else
            use e = source.GetEnumerator()
            IEnumerator.tryItem i e

namespace Microsoft.FSharp.Control

    [<assembly: AutoOpen("Microsoft.FSharp.Control")>]
    do()

    [<AutoOpen>]
    module WebExtensions =
        open System

        type System.Net.WebClient with
            member inline private this.Download(event: IEvent<'T, _>, handler: _ -> 'T, start, result) =
                let downloadAsync =
                    Async.FromContinuations (fun (cont, econt, ccont) ->
                        let userToken = new obj()
                        let rec delegate' (_: obj) (args : #ComponentModel.AsyncCompletedEventArgs) =
                            // ensure we handle the completed event from correct download call
                            if userToken = args.UserState then
                                event.RemoveHandler handle
                                if args.Cancelled then
                                    ccont (new OperationCanceledException())
                                elif args.Error <> null then
                                    econt args.Error
                                else
                                    cont (result args)
                        and handle = handler delegate'
                        event.AddHandler handle
                        start userToken
                    )

                async {
                    use! _holder = Async.OnCancel(fun _ -> this.CancelAsync())
                    return! downloadAsync
                 }

            [<CompiledName("AsyncDownloadData")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
            member this.AsyncDownloadData (address:Uri) : Async<byte[]> =
                this.Download(
                    event   = this.DownloadDataCompleted,
                    handler = (fun action    -> Net.DownloadDataCompletedEventHandler(action)),
                    start   = (fun userToken -> this.DownloadDataAsync(address, userToken)),
                    result  = (fun args      -> args.Result)
                )

            [<CompiledName("AsyncDownloadFile")>] // give the extension member a 'nice', unmangled compiled name, unique within this module
            member this.AsyncDownloadFile (address:Uri, fileName:string) : Async<unit> =
                this.Download(
                    event   = this.DownloadFileCompleted,
                    handler = (fun action    -> ComponentModel.AsyncCompletedEventHandler(action)),
                    start   = (fun userToken -> this.DownloadFileAsync(address, fileName, userToken)),
                    result  = (fun _         -> ())
                )
