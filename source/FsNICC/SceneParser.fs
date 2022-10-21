module FsNICC.SceneParser

open IndentedStreamWriter
open BinaryParser

open System.Numerics

type [<Struct>] RGB         =
  {
    Red     : byte
    Green   : byte
    Blue    : byte
  }
type [<Struct>] Vertex2D    =
  {
    X   : byte
    Y   : byte
  }
type [<Struct>] ColorIndex  = 
  {
    Index   : byte
  }
type [<Struct>] PaletteItem = 
  {
    ColorIndex  : ColorIndex
    Color       : RGB
  }
type [<Struct>] Polygon     = 
  {
    ColorIndex  : ColorIndex
    IsConvex    : bool
    Vertices    : Vertex2D array
  }
type [<Struct>] Frame =
  {
    ClearScreen   : bool
    PaletteDelta  : PaletteItem array
    Polygons      : Polygon array
  }
type [<Struct>] Scene =
  {
    Frames        : Frame array
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

module SceneWriter =
  open IndentedOutput

  let iwritePaletteItem (pi : PaletteItem) =
    ioutput {
      do! iline "PaletteItem" 
      do! iindent 
            (ioutput {
              let c = pi.Color
              do! ilinef "ColorIndex: %d" pi.ColorIndex.Index
              do! ilinef "RGB       : %x%x%x" c.Red c.Green c.Blue
            })
    }

  let iwriteVertex (v: Vertex2D) =
    ioutput {
      do! ilinef "Vertex: %03d, %03d" v.X v.Y
    }

  let iwritePolygon (p: Polygon) =
    ioutput {
      do! iline "Polygon" 
      do! iindent 
            (ioutput {
                do! ilinef "ColorIndex: %d" p.ColorIndex.Index
                do! ilinef "IsConvex  : %A" p.IsConvex
                do! ilinef "Vertices  : %d" p.Vertices.Length
                do! iindent (iiter p.Vertices iwriteVertex)
            })
    }

  let iwriteFrame (f : Frame) =
    ioutput {
      do! iline "Frame"
      do! iindent
            (ioutput {
              do! ilinef "ClearScreen : %A" f.ClearScreen
              do! ilinef "PaletteDelta: %d" f.PaletteDelta.Length
              do! iindent (iiter f.PaletteDelta iwritePaletteItem)
              do! ilinef "Polygons    : %d" f.Polygons.Length
              do! iindent (iiter f.Polygons iwritePolygon)
            })
    }
  let iwriteScene (s : Scene) =
    ioutput {
      do! iline "Scene"
      do! iindent
            (ioutput {
              do! ilinef "Frames: %d" s.Frames.Length
              do! iindent (iiter s.Frames iwriteFrame)
            })
    }

module SceneReader =
  open BinaryReader

  let inline brgb () =
    breader {
      let inline cp c i = 
        let c = int c
        let cp    = (c >>> i) &&& 0xF
        // Atari ST color
        let c     = (cp &&& 0x7) <<< 1
        // Atari STE extended color
        let c     = c + ((cp >>> 3) &&& 0x1)
        byte c

      let! c = bword ()
      return { Red = cp c 8; Green = cp c 4; Blue = cp c 0 }
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
            if (bitmask &&& 0x8000u) <> 0u then
              pds.[i] <- { ColorIndex = { Index = j }; Color = rgbs.[i] }
              i + 1
            else
              i
          loop (bitmask <<< 1) i (j + 1uy) rgbs pds
      loop bitmask 0 0uy rgbs pds
      return pds
    }

  type 'T PolygonDescriptor =
    | IsPolygon of ColorIndex*byte
    | IsExit    of ('T -> 'T ReadResult)
  
  let inline bvertex () =
    breader {
      let! x = bbyte ()
      and! y = bbyte ()
      return { X = x; Y = y }
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
          Continue ({ Index = ci }, vc)
    }

  let isConvex (vs : Vertex2D array) =
    if vs.Length > 3 then
      let inline v3 (v : Vertex2D) = Vector3 (float32 v.X, float32 v.Y, 0.F)
      let sign (v0 : Vertex2D) (v1 : Vertex2D) (v2 : Vertex2D) =
        let x = v3 v0 - v3 v1
        let y = v3 v2 - v3 v1
        let z = Vector3.Cross (x, y)
        z.Z > 0.F
      let rec loop (vs : _ array) s i =
        if i < vs.Length then
          let ns = sign vs.[i - 1] vs.[i] vs.[(i + 1)%vs.Length]
          if ns <> s then 
            false
          else
            loop vs s (i + 1)
        else
          true
      loop vs (sign vs.[vs.Length - 1] vs.[0] vs.[1]) 1
    else
      true

  let bpolygon =
    brepeatPrefixed 
      (bpolygonDescriptor ())
      (fun (ci, vc) -> 
        breader {
          let! vs = bvertices (int vc)
          return { ColorIndex = ci; IsConvex = isConvex vs; Vertices = vs }
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
        brepeatPrefixed 
          (bpolygonDescriptor ())
          (fun (ci, vc) -> 
            breader {
              let! vs = bindexedVertices vs (int vc)
              return { ColorIndex = ci; IsConvex = isConvex vs; Vertices = vs }
            }
          )
    }

  let bnextPage =
    breader {
      let pageSize = 1<<<16
      let! pos  = bgetPos ()
      let pos   = pos + pageSize
      let pos   = pos &&& ~~~(pageSize - 1)
      do! bsetPos pos
    }

  let bframe : Frame UntilResult BinaryReader =
    breader {
      let! flags        = bbyte ()
      let clearScreen   = (flags &&& 0x1uy) <> 0uy
      let hasPalette    = (flags &&& 0x2uy) <> 0uy
      let indexedMode   = (flags &&& 0x4uy) <> 0uy
      let! paletteDelta = if hasPalette then bpaletteDelta else bvalue [||]
      let! polygons     = if indexedMode then bindexedPolygon else bpolygon
      let ps, nf, c =
        match polygons with
        | Next      ps -> ps, false, true
        | NextPage  ps -> ps, true , true
        | Done      ps -> ps, false, false
      let frame = 
        {
          ClearScreen   = clearScreen
          PaletteDelta  = paletteDelta
          Polygons      = ps
        }
      do! if nf then bnextPage else bvalue ()

      return
        {
          Continue = c
          Value    = ValueSome frame
        }
    }
  let bscene : Scene BinaryReader =
    breader {
      let! frames = brepeatUntil bframe
      return { Frames = frames }
    }
