open System
open System.IO

open FSharp.Core.Printf

let log cc msg =
  let bcc = Console.ForegroundColor
  Console.ForegroundColor <- cc
  try
    Console.WriteLine (msg : string)
  finally
    Console.ForegroundColor <- bcc

let good msg = log ConsoleColor.Green   msg
let hili msg = log ConsoleColor.Cyan    msg
let info msg = log ConsoleColor.Gray    msg
let warn msg = log ConsoleColor.Yellow  msg
let fail msg = log ConsoleColor.Red     msg

let goodf fmt = kprintf good fmt
let hilif fmt = kprintf hili fmt
let infof fmt = kprintf info fmt
let warnf fmt = kprintf warn fmt
let failf fmt = kprintf fail fmt

type IndentContext =
  {
    IndentBy  : int
  }

type 'T IndentedOutput = IndentContext -> StreamWriter -> int -> 'T
module IndentedOutput =
  let irun indentBy sw (io : 'T IndentedOutput) = 
    let ctx = {IndentBy = indentBy}
    io ctx sw 0

  let inline ivalue v : 'T IndentedOutput = fun ctx sw i ->
    v

  let inline iindent 
    ([<InlineIfLambda>] io  : 'T IndentedOutput ) 
    : 'T IndentedOutput = fun ctx sw i ->
    io ctx sw (i + ctx.IndentBy)

  let iline (msg : string) : unit IndentedOutput = fun ctx sw i ->
    sw.WriteLine ()
    if i > 0 then
      sw.Write (new String (' ', i))
    sw.Write msg

  let ilinef fmt = kprintf iline fmt

  type IndentedOutputBuilder () =
    class
      member inline x.Bind  ( [<InlineIfLambda>] t  : _ IndentedOutput
                            , [<InlineIfLambda>] uf : _ -> _ IndentedOutput
                            ) : _ IndentedOutput = fun ctx sw i ->
        uf (t ctx sw i) ctx sw i

      member inline x.Combine ( [<InlineIfLambda>] t : _ IndentedOutput
                              , [<InlineIfLambda>] u : _ IndentedOutput
                              ) : _ IndentedOutput =  fun ctx sw i ->
        t ctx sw i
        u ctx sw i

      member inline x.For     ( vs                    : 'T seq
                              , [<InlineIfLambda>] uf : 'T -> unit IndentedOutput
                              ) : unit IndentedOutput =  fun ctx sw i ->
        for v in vs do
          uf v ctx sw i

      member inline x.Return  ( v : 'T
                              ) : 'T IndentedOutput =
        ivalue v
      member inline x.ReturnFrom  (io : 'T IndentedOutput
                                  ): 'T IndentedOutput = 
        io
    end

  let ioutput = IndentedOutputBuilder ()

type 'T BinaryReader = byte array -> int -> 'T*int
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
    let v, _ = br bs 0
    v

  let bdebug nm (br : _ BinaryReader) : _ BinaryReader = fun bs i ->
    infof "bdebug - BEFORE - %s - %x" nm i
    let bv, bi = br bs i
    infof "bdebug - AFTER  - %s - %x - %A" nm bi bv
    bv, bi

  let inline bvalue v : _ BinaryReader = fun bs i ->
    v, i

  let inline bgetPos () : int BinaryReader = fun bs i ->
    i, i

  let inline bsetPos pos : unit BinaryReader = fun bs i ->
    (), pos

  let inline bbyte () : uint8 BinaryReader = fun bs i ->
    bs.[i], i + 1

  let inline bword () : uint16 BinaryReader = fun bs i ->
    let word = 
        ((uint16 bs.[i]) <<< 8) 
      + (uint16 bs.[i +  1])
    word, i + 2

  let inline brepeat 
    (count                  : int             )
    ([<InlineIfLambda>] br  : 'T BinaryReader )
    : 'T array BinaryReader = fun bs i ->
    let vs = Array.zeroCreate count

    let mutable ii  = i
    let mutable idx = 0
    while idx < count do
      let nv, ni = br bs ii
      vs.[idx] <- nv
      ii  <- ni
      idx <- idx + 1
    vs, ii

  type PrefixAction<'S, 'T, 'U> =
    | Continue  of  'S
    | Stop      of  ('T -> 'U)

  let inline brepeatPrefix 
    ([<InlineIfLambda>] t   : PrefixAction<'S, 'T array, 'U> BinaryReader )
    ([<InlineIfLambda>] uf  : 'S -> 'T BinaryReader                       )
    : 'U BinaryReader = fun bs i ->
    let vs : 'T ResizeArray = ResizeArray 16

    let mutable rr  = Unchecked.defaultof<_>
    let mutable cc  = true
    let mutable ii  = i
    while cc do
      let tv, ti = t bs ii
      match tv with
      | Continue  sv ->
        let uv, ui = uf sv bs ti
        vs.Add uv
        ii <- ui
      | Stop      sf ->
        rr <- sf (vs.ToArray ())
        cc <- false
        ii <- ti
    rr, ii

  type BinaryReaderBuilder () =
    class
      member inline x.Bind  ( [<InlineIfLambda>] t  : _ BinaryReader
                            , [<InlineIfLambda>] uf : _ -> _ BinaryReader
                            ) : _ BinaryReader = fun bs i ->
        let tv, ti = t bs i
        uf tv bs ti

      member inline x.Combine ( [<InlineIfLambda>] t : _ BinaryReader
                              , [<InlineIfLambda>] u : _ BinaryReader
                              ) : _ BinaryReader =  fun bs i ->
        let _, ti = t bs i
        u bs ti

      member inline x.Return  ( v : 'T
                              ) : 'T BinaryReader =
        bvalue v
      member inline x.ReturnFrom  (br : 'T BinaryReader
                                  ): 'T BinaryReader = 
        br
    end
  let breader = BinaryReaderBuilder()

type [<Struct>] RGB         = RGB         of byte*byte*byte
type [<Struct>] Vertex2D    = Vertex2D    of byte*byte
type [<Struct>] ColorIndex  = ColorIndex  of byte
type [<Struct>] PaletteItem = PaletteItem of ColorIndex*RGB
type [<Struct>] Polygon     = Polygon     of ColorIndex*Vertex2D array
type [<Struct>] Frame =
  {
    ClearScreen   : bool
    PaletteDelta  : PaletteItem array
    Polygons      : Polygon array
  }

type 'T ReadResult =
  | Next      of 'T
  | NextPage  of 'T
  | Done      of 'T

  member x.Map f =
    match x with
    | Next      v -> Next      (f v)
    | NextPage  v -> NextPage  (f v)
    | Done      v -> Done      (f v)

module FrameReader =
  open BinaryReader

  let toArray vs = vs |> List.rev |> List.toArray

  let brgb () =
    breader {
      let! c = bword ()
      let c = int c
      let inline cp i = 
        let cp    = (c >>> i) &&& 0xF
        // Atari ST color
        let c     = (cp &&& 0x7) <<< 1
        // Atari STE extended color
        let c     = c + ((cp >>> 3) &&& 0x1)
        byte c

      return RGB (cp 8, cp 4, cp 0)
    }

  let bpaletteDelta : PaletteItem array BinaryReader =
    breader {
      let! bitmask  = bword ()
      let bitmask   = uint32 bitmask
      let bitCount  = popCount bitmask |> int
      let! rgbs     = brepeat bitCount (brgb ())
      let pds       = Array.zeroCreate bitCount
      // Zip the bit selection with the rgb values
      let rec loop bitmask i j (rgbs : _ array) (pds : _ array)=
        if bitmask <> 0u then
          let i = 
            if (bitmask &&& 0x1u) <> 0u then
              pds.[i] <- PaletteItem (ColorIndex j, rgbs.[i])
              i + 1
            else
              i
          loop (bitmask >>> 1) i (j - 1uy) rgbs pds
      loop bitmask 0 15uy rgbs pds
      return pds
    }

  type 'T PolygonDescriptor =
    | IsPolygon of ColorIndex*byte
    | IsExit    of ('T -> 'T ReadResult)
  
  let inline bvertex () =
    breader {
      let! x = bbyte ()
      let! y = bbyte ()
      return Vertex2D (x,y)
    }
  let bvertices c = brepeat c (bvertex ())

  let bpolygonDescriptor () =
    breader {
      let! pd = bbyte ()
      return
        match pd with
        | 0xFFuy  -> Stop Next
        | 0xFEuy  -> Stop NextPage
        | 0xFDuy  -> Stop Done
        | _       ->
          let ci = (pd >>> 4) &&& 0xFuy
          let vc = pd &&& 0xFuy
          let vc = vc
          Continue (ColorIndex ci, vc)
    }

  let bpolygon =
    brepeatPrefix 
      (bpolygonDescriptor ())
      (fun (ci, vc) -> 
        breader {
          let! vs = bvertices (int vc)
          return Polygon (ci, vs)
        }
      )

  let inline bindexedVertex (vs : Vertex2D array) =
    breader {
      let! idx = bbyte ()
      return vs.[int idx]
    }

  let bindexedVertices vs c = brepeat c (bindexedVertex vs)

  let bindexedPolygon =
    breader {
      let! vc = bbyte ()
      let! vs = bvertices (int vc)
      return!
        brepeatPrefix 
          (bpolygonDescriptor ())
          (fun (ci, vc) -> 
            breader {
              let! vs = bindexedVertices vs (int vc)
              return Polygon (ci, vs)
            }
          )
    }

  let bnextPage () =
    breader {
      let pageSize = 1<<<16
      let! pos  = bgetPos ()
      let mpos  = (pos + 1) % 65536
      let pos   = pos + pageSize - mpos
      do! bsetPos pos
    }

  let bframe : Frame ReadResult BinaryReader =
    breader {
      let! flags        = bbyte ()
      let clearScreen   = (flags &&& 0x1uy) <> 0uy
      let hasPalette    = (flags &&& 0x2uy) <> 0uy
      let indexedMode   = (flags &&& 0x4uy) <> 0uy
      let! paletteDelta = if hasPalette then bpaletteDelta else bvalue [||]
      let! polygons     = if indexedMode then bindexedPolygon else bpolygon
      let mapper ps =
        {
          ClearScreen   = clearScreen
          PaletteDelta  = paletteDelta
          Polygons      = ps
        }
      return polygons.Map mapper
    }

open IndentedOutput
let iwritePaletteDelta (pd : PaletteItem array) =
  ioutput {
    for pi in pd do
      let (PaletteItem (ColorIndex ci, RGB (r, g, b))) = pi
      do! ilinef "ColorIndex: %d" ci
      do! ilinef "RGB       : %x%x%x" r g b
  }

let iwriteVertices (vs: Vertex2D array) =
  ioutput {
    for v in vs do
      let (Vertex2D (x, y)) = v
      do! ilinef "X: %d" x
      do! ilinef "Y: %d" y
  }

let iwritePolygons (ps: Polygon array) =
  ioutput {
    for p in ps do
      let (Polygon (ColorIndex ci, vs)) = p
      do! ilinef "ColorIndex: %d" ci
      do! ilinef "Vertices  : %d" vs.Length
      do! iindent (iwriteVertices vs)
  }
let iwriteFrame (f : Frame) =
  ioutput {
    do! iline "Frame"
    do! iindent
          (ioutput {
            do! ilinef "ClearScreen : %A" f.ClearScreen
            do! ilinef "PaletteDelta: %d" f.PaletteDelta.Length
            do! iindent (iwritePaletteDelta f.PaletteDelta)
            do! ilinef "Polygons    : %d" f.Polygons.Length
            do! iindent (iwritePolygons f.Polygons)
          })
  }
let iwriteFrameReadResult (fr : Frame ReadResult) =
  ioutput {
    let msg, f = 
      match fr with
      | Next      f -> "Next"     , f
      | NextPage  f -> "NextPage" , f
      | Done      f -> "Done"     , f
    do! ilinef "Frame ReadResult: %s" msg
    do! iindent (iwriteFrame f)
  }

let go () =
  Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
  let input   = Path.GetFullPath "scene1.bin"
  let output  = Path.GetFullPath "../../../../../assets/scene1.txt"

  hilif "Reading scene: %s" input
  let bs = File.ReadAllBytes input

  hilif "Parsing scene: %s" input
  let result = BinaryReader.brun FrameReader.bframe bs

  hilif "Writing scene: %s" output
  use sw = File.CreateText output
  irun 2 sw (iwriteFrameReadResult result)

  ()

[<EntryPoint>]
let main args =
  try
    go ()
    0
  with
  | e -> 
    printfn "%s" ""
    99
