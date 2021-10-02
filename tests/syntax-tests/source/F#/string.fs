// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Core

    open System
    open System.Text
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.Operators.Checked
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module String =
        [<Literal>]
        /// LOH threshold is calculated from Internal.Utilities.Library.LOH_SIZE_THRESHOLD_BYTES,
        /// and is equal to 80_000 / sizeof<char>
        let LOH_CHAR_THRESHOLD = 40_000

        [<CompiledName("Length")>]
        let length (str:string) = if isNull str then 0 else str.Length

        [<CompiledName("Concat")>]
        let concat sep (strings : seq<string>) =  

            let concatArray sep (strings: string []) =
                match length sep with
                | 0 -> String.Concat strings
                // following line should be used when this overload becomes part of .NET Standard (it's only in .NET Core)
                //| 1 -> String.Join(sep.[0], strings, 0, strings.Length)
                | _ -> String.Join(sep, strings, 0, strings.Length)

            match strings with
            | :? array<string> as arr -> 
                concatArray sep arr

            | :? list<string> as lst -> 
                lst 
                |> List.toArray 
                |> concatArray sep

            | _ ->
                String.Join(sep, strings)

        [<CompiledName("Iterate")>]
        let iter (action : (char -> unit)) (str:string) =
            if not (String.IsNullOrEmpty str) then
                for i = 0 to str.Length - 1 do
                    action str.[i] 

        [<CompiledName("IterateIndexed")>]
        let iteri action (str:string) =
            if not (String.IsNullOrEmpty str) then
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(action)
                for i = 0 to str.Length - 1 do
                    f.Invoke(i, str.[i]) 

        [<CompiledName("Map")>]
        let map (mapping: char -> char) (str:string) =
            if String.IsNullOrEmpty str then
                String.Empty
            else
                let result = str.ToCharArray()
                let mutable i = 0
                for c in result do
                    result.[i] <- mapping c
                    i <- i + 1

                new String(result)

        [<CompiledName("MapIndexed")>]
        let mapi (mapping: int -> char -> char) (str:string) =
            let len = length str
            if len = 0 then 
                String.Empty
            else
                let result = str.ToCharArray()
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(mapping)

                let mutable i = 0
                while i < len do
                    result.[i] <- f.Invoke(i, result.[i])
                    i <- i + 1

                new String(result)

        [<CompiledName("Filter")>]
        let filter (predicate: char -> bool) (str:string) =
            let len = length str

            if len = 0 then 
                String.Empty

            elif len > LOH_CHAR_THRESHOLD then
                // By using SB here, which is twice slower than the optimized path, we prevent LOH allocations 
                // and 'stop the world' collections if the filtering results in smaller strings.
                // We also don't pre-allocate SB here, to allow for less mem pressure when filter result is small.
                let res = StringBuilder()
                str |> iter (fun c -> if predicate c then res.Append c |> ignore)
                res.ToString()

            else
                // Must do it this way, since array.fs is not yet in scope, but this is safe
                let target = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len
                let mutable i = 0
                for c in str do
                    if predicate c then 
                        target.[i] <- c
                        i <- i + 1

                String(target, 0, i)

        [<CompiledName("Collect")>]
        let collect (mapping: char -> string) (str:string) =
            if String.IsNullOrEmpty str then
                String.Empty
            else
                let res = StringBuilder str.Length
                str |> iter (fun c -> res.Append(mapping c) |> ignore)
                res.ToString()

        [<CompiledName("Initialize")>]
        let init (count:int) (initializer: int-> string) =
            if count < 0 then invalidArgInputMustBeNonNegative "count" count
            let res = StringBuilder count
            for i = 0 to count - 1 do 
               res.Append(initializer i) |> ignore
            res.ToString()

        [<CompiledName("Replicate")>]
        let replicate (count:int) (str:string) =
            if count < 0 then invalidArgInputMustBeNonNegative "count" count

            let len = length str
            if len = 0 || count = 0 then 
                String.Empty

            elif len = 1 then
                new String(str.[0], count)

            elif count <= 4 then
                match count with
                | 1 -> str
                | 2 -> String.Concat(str, str)
                | 3 -> String.Concat(str, str, str)
                | _ -> String.Concat(str, str, str, str)

            else
                // Using the primitive, because array.fs is not yet in scope. It's safe: both len and count are positive.
                let target = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (len * count)
                let source = str.ToCharArray()

                // O(log(n)) performance loop:
                // Copy first string, then keep copying what we already copied 
                // (i.e., doubling it) until we reach or pass the halfway point
                Array.Copy(source, 0, target, 0, len)
                let mutable i = len
                while i * 2 < target.Length do
                    Array.Copy(target, 0, target, i, i)
                    i <- i * 2

                // finally, copy the remain half, or less-then half
                Array.Copy(target, 0, target, i, target.Length - i)
                new String(target)


        [<CompiledName("ForAll")>]
        let forall predicate (str:string) =
            if String.IsNullOrEmpty str then
                true
            else
                let rec check i = (i >= str.Length) || (predicate str.[i] && check (i+1)) 
                check 0

        [<CompiledName("Exists")>]
        let exists predicate (str:string) =
            if String.IsNullOrEmpty str then
                false
            else
                let rec check i = (i < str.Length) && (predicate str.[i] || check (i+1)) 
                check 0  
