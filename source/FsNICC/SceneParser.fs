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
type [<Struct>] Triangle2D  =
  {
    V0  : Vertex2D
    V1  : Vertex2D
    V2  : Vertex2D
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
    IsSimple    : bool
    Vertices    : Vertex2D array
    Simplified  : Vertex2D array array
    Triangulated: Triangle2D array
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

  let inline iwritePaletteItem (pi : PaletteItem) =
    ioutput {
      do! iline "PaletteItem"
      do! iindent
            (ioutput {
              let c = pi.Color
              do! ilinef "ColorIndex: %d" pi.ColorIndex.Index
              do! ilinef "RGB       : %x%x%x" c.Red c.Green c.Blue
            })
    }

  let inline iwriteVertices (vs: Vertex2D array) =
    ioutput {
      do! ilinef "Vertices  : %d" vs.Length
      do! iindent (iiter vs (fun v -> ilinef "Vertex: %03d, %03d" v.X v.Y))
    }
  let iwritePolygon (p: Polygon) =
    ioutput {
      do! iline "Polygon"
      do! iindent
            (ioutput {
                do! ilinef "ColorIndex: %d" p.ColorIndex.Index
                do! ilinef "IsConvex  : %A" p.IsConvex
                do! ilinef "IsSimple  : %A" p.IsSimple
                do! iwriteVertices p.Vertices
                do! ilinef "Simplified: %d" p.Simplified.Length
                do! iindent (iiter p.Simplified iwriteVertices)
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

  type [<Struct>] Line =
    {
      P0 :  Vector2
      P1 :  Vector2
    }

  let intersectPoint (l0 : Line) (l1 : Line) : Vector2 =
    let x1  = l0.P0.X
    let y1  = l0.P0.Y
    let x2  = l0.P1.X
    let y2  = l0.P1.Y
    let x3  = l1.P0.X
    let y3  = l1.P0.Y
    let x4  = l1.P1.X
    let y4  = l1.P1.Y
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
    let d   = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
    if abs d < System.Single.Epsilon then
      Vector2 (nanf, nanf)
    else
      let e   = x1*y2-y1*x2
      let f   = x3*y4-y3*x4
      let px  = (e*(x3-x4)-f*(x1-x2))/d
      let py  = (e*(y3-y4)-f*(y1-y2))/d
      Vector2 (px, py)

  let isInsideBoundingBox (l : Line) (p : Vector2) =
    let n = Vector2.Min (l.P0, l.P1)
    let x = Vector2.Max (l.P0, l.P1)
    n.X < p.X && n.Y < p.Y && p.X < x.X && p.Y < x.Y

  let isIntersecting (l0 : Line) (l1 : Line) : bool =
    let p = intersectPoint l0 l1
    isInsideBoundingBox l0 p && isInsideBoundingBox l1 p

  let toVector2 (v : Vertex2D) : Vector2 = Vector2(float32 v.X, float32 v.Y)

  let toVertex2D (v : Vector2) : Vertex2D =
    let inline r x = byte (round x)
    { X = r v.X; Y = r v.Y }

  let toLines (vs : Vertex2D array) : Line array =
    match vs.Length with
    | 0 -> [||]
    | 1 -> [|{ P0 = toVector2 vs.[0]; P1 = toVector2 vs.[0]; }|]
    | _ ->
      let rec loop b (vs : Vertex2D array) (res : Line array) i =
        if i < vs.Length then
          let c = toVector2 vs.[i]
          res.[i] <- { P0 = b; P1 = c }
          loop c vs res (i + 1)
        else
          res
      let b   = toVector2 vs.[vs.Length - 1]
      let res = Array.zeroCreate vs.Length
      loop b vs res 0

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

  let isSimple (vs : Vertex2D array) =
    if vs.Length > 3 then
      let ls = toLines vs
      let rec loop (ls : Line array) i =
        if i + 1 < ls.Length then
          let c = ls.[i]
          let rec iloop (ls : Line array) c i =
            if i < ls.Length then
              let l = ls.[i]
              if isIntersecting c l then
                true
              else
                iloop ls c (i + 1)
            else false
          let intersects = iloop ls c (i + 1)
          if intersects then
            false
          else
            loop ls (i + 1)
        else
          true
      loop ls 0
    else
      true

  let makeSimple vs =
    let split (vs : Vertex2D array) p f s =
      let f, s = min f s, max f s
      let vs0 = [|vs.[f..(s-1)]; [|p|]|] |> Array.concat
      let vs1 = [|vs.[0..(f-1)] ; [|p|]; vs.[s..(vs.Length - 1)]|] |> Array.concat

      struct (vs0, vs1)

    let rec oloop res (vs : Vertex2D array) =
      if vs.Length > 3 then
        let ls = toLines vs
        let rec loop res vs (ls : Line array) i =
          if i + 1 < ls.Length then
            let c = ls.[i]
            let rec iloop res vs (ls : Line array) c j =
              if j < ls.Length then
                let l = ls.[j]
                let ip = intersectPoint c l
                if isInsideBoundingBox c ip && isInsideBoundingBox l ip then
                  let struct (vs0, vs1) = split vs (toVertex2D ip) i j
                  let res = oloop res vs0
                  let res = oloop res vs1
                  struct (true, res)
                else
                  iloop res vs ls c (j + 1)
              else
                struct (false, res)
            let struct (stop, res) = iloop res vs ls c (i + 1)
            if stop then
              res
            else
              loop res vs ls (i + 1)
          else
            vs::res
        loop res vs ls 0
      else
        vs::res
    oloop [] vs
    |> List.rev
    |> List.toArray
  let polygon ci vs =
    let c = isConvex vs
    let s = isSimple vs
    let ss= if s then [||] else makeSimple vs
    let ts= [||]
    {
      ColorIndex  = ci
      IsConvex    = c
      IsSimple    = s
      Vertices    = vs
      Simplified  = ss
      Triangulated= ts
    }

  let bpolygon =
    brepeatPrefixed
      (bpolygonDescriptor ())
      (fun (ci, vc) ->
        breader {
          let! vs = bvertices (int vc)
          return polygon ci vs
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
              return polygon ci vs
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
