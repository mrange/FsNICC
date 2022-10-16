open System
open System.IO
open System.Windows
open System.Windows.Media
open System.Windows.Media.Animation

open System.Collections.Generic

open Log
open IndentedStreamWriter
open BinaryParser

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

      return { Red = cp 8; Green = cp 4; Blue = cp 0 }
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
      let! y = bbyte ()
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

  let bpolygon =
    brepeatPrefixed 
      (bpolygonDescriptor ())
      (fun (ci, vc) -> 
        breader {
          let! vs = bvertices (int vc)
          return { ColorIndex = ci; Vertices = vs }
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
              return { ColorIndex = ci; Vertices = vs }
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

type [<Struct>] WPFPolygon =
  {
    Fill  : Brush
    Path  : PathGeometry
  }

type [<Struct>] WPFFrame =
  {
    Polygons  : WPFPolygon array
  }

type [<Struct>] WPFScene =
  {
    Frames : WPFFrame array
  }

type SceneElement (scene : WPFScene) =
  class
    inherit UIElement ()

    static let timeProperty =
      let pc = PropertyChangedCallback SceneElement.TimePropertyChanged
      let md = PropertyMetadata (0., pc)
      DependencyProperty.Register ("Time", typeof<float>, typeof<SceneElement>, md)

    let freeze (f : #Freezable) = 
      f.Freeze ()
      f

    let makePen thickness brush = 
      Pen (Thickness = thickness, Brush = brush) |> freeze

    let particlePen = makePen 2. Brushes.White
    let stickPen    = makePen 2. Brushes.Yellow
    let ropePen     = makePen 2. Brushes.GreenYellow

    static member TimePropertyChanged (d : DependencyObject) (e : DependencyPropertyChangedEventArgs) =
      let g = d :?> SceneElement
      g.InvalidateVisual ()

    static member TimeProperty = timeProperty

    member x.Time = x.GetValue SceneElement.TimeProperty :?> float

    member x.Start () =
      let b   = 0.
      let e   = 1E9
      let dur = Duration (TimeSpan.FromSeconds (e - b))
      let ani = DoubleAnimation (b, e, dur) |> freeze
      x.BeginAnimation (SceneElement.TimeProperty, ani);

    override x.OnRender dc =
      let time  = x.Time
      let rs    = x.RenderSize
      let c     = Point (rs.Width*0.5, rs.Height*0.5)

      let t     = ScaleTransform (3.0, 3.0)
      dc.PushTransform t
      let i     = int (floor (time * 25.0))
      let i     = i % scene.Frames.Length
      let f     = scene.Frames.[i]
      for p in f.Polygons do
        dc.DrawGeometry (p.Fill, null, p.Path)

      dc.Pop ()
  end

let toWPFScene (scene : Scene) : WPFScene =
  let freeze (f : #Freezable) = 
    f.Freeze ()
    f

  let frames = scene.Frames

  let palette       : RGB array               = Array.zeroCreate 16
  let knownBrushes  : Dictionary<RGB, Brush>  = Dictionary<RGB, Brush> ()

  let mapPolygon (f : Polygon) : WPFPolygon =
    let rgb   = palette.[int f.ColorIndex.Index]

    let brush = knownBrushes.[rgb]
    let vs    = f.Vertices
    let pts   = Array.zeroCreate (vs.Length + 1)
    let rec loop (vs : Vertex2D array) (pts : Point array) i =
      if i < vs.Length then
        let v = vs.[i]
        pts.[i] <- Point (float v.X, float v.Y)
        loop vs pts (i + 1)
      else
        let v = vs.[0]
        pts.[i] <- Point (float v.X, float v.Y)

    loop vs pts 0

    let pls   = PolyLineSegment (
        pts
      , false
      )
    let pls   = freeze pls

    let pf    = PathFigure ()
    pf.Segments.Add pls
    let pf    = freeze pf

    let pg    = PathGeometry ()
    pg.Figures.Add pf
    let pg = freeze pg

    { Fill = brush; Path = pg }
  let mapFrame (f : Frame) : WPFFrame =
    for pi in f.PaletteDelta do
      palette.[int pi.ColorIndex.Index] <- pi.Color

    for color in palette do
      match knownBrushes.TryGetValue color with
      | true  , _ -> ()
      | false , _ ->
        let c = Color.FromRgb ( color.Red   <<< 4
                              , color.Green <<< 4
                              , color.Blue  <<< 4
                              )
        let b = SolidColorBrush c
        let b = freeze b
        knownBrushes.Add (color, b)

    let polygons = f.Polygons |> Array.map mapPolygon

    { Polygons = polygons }


  let wpfFrames = frames |> Array.map mapFrame

  { Frames = wpfFrames }

let run () =
  Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
  let input   = Path.GetFullPath "scene1.bin"
  let output  = Path.GetFullPath "../../../../../assets/scene1.txt"

  hilif "Reading scene: %s" input
  let bs = File.ReadAllBytes input

  hilif "Parsing scene: %s" input
  let scene = BinaryReader.brun SceneReader.bscene bs

  hilif "Writing scene: %s" output
  use sw = File.CreateText output
  IndentedOutput.irun 2 sw (SceneWriter.iwriteScene scene)

  hilif "Making WPF Scene with %d frames" scene.Frames.Length
  let wpfScene = toWPFScene scene

  hili "Launching scene render window"
  let window  = Window (Title = "FsNICC", Background = Brushes.Black)
  let element = SceneElement wpfScene
  window.Content <- element
  element.Start ()
  window.ShowDialog () |> ignore

  ()

[<EntryPoint>]
[<STAThread>]
let main args =
  try
    run ()
    0
  with
  | e -> 
    failf "Caught exception: %s\n%s" e.Message (e.ToString())
    99
