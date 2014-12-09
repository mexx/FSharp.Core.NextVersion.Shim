namespace Microsoft.FSharp.Core.CompilerServices
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
    module internal SR =
        let keyNotFoundAlt = "An index satisfying the predicate was not found in the collection."
        let arrayWasEmpty = "The input array was empty."
        let inputSequenceEmpty = "The input sequence was empty."
        let inputMustBeNonNegative = "The input must be non-negative."
        let notEnoughElements = "The input sequence has an insufficient number of elements."
        let inputSequenceTooLong = "The input sequence contains more than one element."
        let arraysHadDifferentLengths = "The arrays have different lengths."
        let outOfRange = "The index is outside the legal range."

        let GetString(name:System.String) : System.String = name

namespace Microsoft.FSharp.Primitives.Basics

    module internal Array =
        open Microsoft.FSharp.Core

        // The input parameter should be checked by callers if necessary
        let inline zeroCreateUnchecked (count:int) =
            Array.zeroCreate count

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

    module internal List =
        open System.Collections.Generic

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

namespace Local.LanguagePrimitives
    module ErrorStrings =
        open Microsoft.FSharp.Core

        let InputArrayEmptyString = SR.GetString(SR.arrayWasEmpty)
        let InputSequenceEmptyString = SR.GetString(SR.inputSequenceEmpty)

namespace Microsoft.FSharp.Collections

    [<assembly: AutoOpen("Microsoft.FSharp.Collections")>]
    do()

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
    module Seq =

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

        [<CompiledName("TryFindBack")>]
        let tryFindBack f (source : seq<'T>) =
            checkNonNull "source" source
            source |> Seq.toArray |> Array.tryFindBack f

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
