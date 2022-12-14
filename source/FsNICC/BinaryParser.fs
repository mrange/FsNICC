(*
Copyright 2022 Mårten Rånge

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
module FsNICC.BinaryParser
open System.Diagnostics

type 'T BinaryReader = byte array -> int -> struct ('T*int)
module BinaryReader =
  let popCount (i : uint32) : uint32 =
    // From: http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
    //  x86/x64 support popcnt but that isn't available in ILAsm
    //  TODO: This is designed for 32 bit integers but we only need to count in 16 bit integers
    //    Is there a faster way to count then?
    let mutable v = i
    v <- v - ((v >>> 1) &&& 0x55555555u)
    v <- (v &&& 0x33333333u) + ((v >>> 2) &&& 0x33333333u)
    ((v + (v >>> 4) &&& 0xF0F0F0Fu) * 0x1010101u) >>> 24

  let brun (br : 'T BinaryReader) bs : 'T =
    let struct (v, _) = br bs 0
    v

  let bdebug nm (br : _ BinaryReader) : _ BinaryReader = fun bs i ->
    Debug.WriteLine (sprintf "bdebug - BEFORE - %s - %x" nm i)
    let struct (bv, bi) = br bs i
    Debug.WriteLine (sprintf "bdebug - AFTER  - %s - %x - %A" nm bi bv)
    struct (bv, bi)

  let inline bvalue v : _ BinaryReader = fun bs i ->
    struct (v, i)

  let inline bgetPos () : int BinaryReader = fun bs i ->
    struct (i, i)

  let inline bsetPos pos : unit BinaryReader = fun bs i ->
    struct ((), pos)

  let inline bbyte () : uint8 BinaryReader = fun bs i ->
    struct (bs.[i], i + 1)

  let inline bword () : uint16 BinaryReader = fun bs i ->
    let word =
        ((uint16 bs.[i]) <<< 8)
      + (uint16 bs.[i +  1])
    struct (word, i + 2)

  let inline brepeat
    (count                  : int             )
    ([<InlineIfLambda>] br  : 'T BinaryReader )
    : 'T array BinaryReader = fun bs i ->
    let vs = Array.zeroCreate count

    let mutable ii  = i
    let mutable idx = 0
    while idx < count do
      let struct (nv, ni) = br bs ii
      vs.[idx] <- nv
      ii  <- ni
      idx <- idx + 1
    struct (vs, ii)

  type [<Struct>] 'T UntilResult =
    {
      Continue  : bool
      Value     : 'T voption
    }
  let inline brepeatUntil
    ([<InlineIfLambda>] br  : 'T UntilResult BinaryReader )
    : 'T array BinaryReader = fun bs i ->
    let vs : 'T ResizeArray = ResizeArray 16

    let mutable ii  = i
    let mutable cc  = true
    while cc do
      let struct (nv, ni) = br bs ii
      match nv.Value with
      | ValueNone     ->
        ()
      | ValueSome nv  ->
        vs.Add nv
      cc <- nv.Continue
      //cc <- false
      ii  <- ni
    struct (vs.ToArray (), ii)

  type PrefixResult<'S, 'T, 'U> =
    | Continue  of  'S
    | Stop      of  ('T -> 'U)

  let inline brepeatPrefixed
    ([<InlineIfLambda>] t   : PrefixResult<'S, 'T array, 'U> BinaryReader )
    ([<InlineIfLambda>] uf  : 'S -> 'T BinaryReader                       )
    : 'U BinaryReader = fun bs i ->
    let vs : 'T ResizeArray = ResizeArray 16

    let mutable rr  = Unchecked.defaultof<_>
    let mutable cc  = true
    let mutable ii  = i
    while cc do
      let struct (tv, ti) = t bs ii
      match tv with
      | Continue  sv ->
        let struct (uv, ui) = uf sv bs ti
        vs.Add uv
        ii <- ui
      | Stop      sf ->
        rr <- sf (vs.ToArray ())
        cc <- false
        ii <- ti
    struct (rr, ii)

  type BinaryReaderBuilder () =
    class
      member inline x.Bind  ( [<InlineIfLambda>] t  : _ BinaryReader
                            , [<InlineIfLambda>] uf : _ -> _ BinaryReader
                            ) : _ BinaryReader = fun bs i ->
        let struct (tv, ti) = t bs i
        uf tv bs ti

      member inline x.Combine ( [<InlineIfLambda>] t : _ BinaryReader
                              , [<InlineIfLambda>] u : _ BinaryReader
                              ) : _ BinaryReader =  fun bs i ->
        let struct (_, ti) = t bs i
        u bs ti

      member inline x.MergeSources  ( [<InlineIfLambda>] t : _ BinaryReader
                                    , [<InlineIfLambda>] u : _ BinaryReader
                                    ) : _ BinaryReader =  fun bs i ->
        let struct (tv, ti) = t bs i
        let struct (uv, ui) = u bs ti
        struct (struct (tv, uv), ui)

      member inline x.BindReturn    ( [<InlineIfLambda>] t : _ BinaryReader
                                    , [<InlineIfLambda>] f
                                    ) : _ BinaryReader =  fun bs i ->
        let struct (tv, ti) = t bs i
        struct (f tv, ti)

      member inline x.Return  ( v : 'T
                              ) : 'T BinaryReader =
        bvalue v
      member inline x.ReturnFrom  (br : 'T BinaryReader
                                  ): 'T BinaryReader =
        br
    end
  let breader = BinaryReaderBuilder()
