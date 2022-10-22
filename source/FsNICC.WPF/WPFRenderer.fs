module FsNICC.WPF.WPFRenderer
open System
open System.Collections.Generic
open System.Globalization
open System.Windows
open System.Windows.Input
open System.Windows.Media
open System.Windows.Media.Animation

open FsNICC
open SceneParser

type [<Struct>] WPFPolygon =
  {
    Fill      : Brush
    IsConvex  : bool
    Path      : PathGeometry
  }

type [<Struct>] WPFFrame =
  {
    Polygons  : WPFPolygon array
  }

type [<Struct>] WPFScene =
  {
    Frames : WPFFrame array
  }

let freeze (f : #Freezable) =
  f.Freeze ()
  f

type SceneElement (scene : WPFScene) =
  class
    inherit UIElement ()

    static let timeProperty =
      let pc = PropertyChangedCallback SceneElement.TimePropertyChanged
      let md = PropertyMetadata (0., pc)
      DependencyProperty.Register ("Time", typeof<float>, typeof<SceneElement>, md)

    let mutable frameNo = 0


    let formattedText text  = FormattedText (
        text
      , CultureInfo.InstalledUICulture
      , FlowDirection.LeftToRight
      , Typeface "Courier New"
      , 32.
      , Brushes.White
      , null
      , TextFormattingMode.Display
      , 1.
      )

    let translateTransform  = TranslateTransform ()
    let scaleTransform      = ScaleTransform ()
    let fullTransform       =
      let t     = TransformGroup ()
      t.Children.Add (TranslateTransform (-128., -100.))
      t.Children.Add scaleTransform
      t.Children.Add translateTransform
      t

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

    override x.OnKeyUp e =
      match e.Key with
      | Key.Up    -> frameNo <- frameNo + 1
      | Key.Down  -> frameNo <- frameNo - 1
      | _         -> ()
      frameNo <- max frameNo 0
      frameNo <- min frameNo (scene.Frames.Length - 1)
      ()

    override x.OnRender dc =
      let time  = x.Time
      let rs    = x.RenderSize

      let s     = min (rs.Width/256.) (rs.Height/200.)

      let dfps  = 30.
#if STEP_FRAME
      let i     = frameNo
#else
      let i     = int (floor (time * dfps))
      let i     = i % scene.Frames.Length
#endif

      let f     = scene.Frames.[i]

      let ft    = formattedText $"Frame: {i}\nTime : %.2f{time}s\nPolys: {f.Polygons.Length}"

      scaleTransform.ScaleX <- s
      scaleTransform.ScaleY <- s
      translateTransform.X  <- rs.Width*0.5
      translateTransform.Y  <- rs.Height*0.5
      dc.PushTransform fullTransform

      for p in f.Polygons do
        dc.DrawGeometry (p.Fill, null, p.Path)

      dc.Pop ()

      dc.DrawText (ft, Point (0., 0.))
  end

let toWPFScene (scene : Scene) : WPFScene =
  let frames = scene.Frames

  let palette       : RGB array               = Array.zeroCreate 16
  let knownBrushes  : Dictionary<RGB, Brush>  = Dictionary<RGB, Brush> ()

  let mapPolygon (f : Polygon) : WPFPolygon =
    let rgb   = palette.[int f.ColorIndex.Index]

    let brush = knownBrushes.[rgb]
    let pf    = PathFigure ()

    let rec loop (pf : PathFigure) (vvs : Vertex2D array array) i = 
      if i < vvs.Length then
        let vs    = vvs.[i]
        let pts   = Array.zeroCreate (vs.Length)
        let rec iloop (vs : Vertex2D array) (pts : Point array) i =
          if i < vs.Length then
            let v = vs.[i]
            pts.[i] <- Point (float v.X, float v.Y)
            iloop vs pts (i + 1)
        iloop vs pts 0

        let pls   = PolyLineSegment (
            pts
          , false
          )
        let pls   = freeze pls

        pf.Segments.Add pls
        loop pf vvs (i + 1)

    let vvs =
      if f.Simplified.Length > 0 then
        f.Simplified
      else
        [|f.Vertices|]
    loop pf vvs 0

    let v0 = vvs.[0].[0]
    pf.StartPoint <- Point (float v0.X, float v0.Y)
    pf.IsClosed   <- true
    let pf    = freeze pf

    let pg    = PathGeometry ()
    pg.FillRule <- FillRule.Nonzero;
    pg.Figures.Add pf
    let pg = freeze pg

    { Fill = brush; IsConvex = f.IsConvex; Path = pg }
  let mapFrame (f : Frame) : WPFFrame =
    for pi in f.PaletteDelta do
      palette.[int pi.ColorIndex.Index] <- pi.Color

    for color in palette do
      match knownBrushes.TryGetValue color with
      | true  , _ -> ()
      | false , _ ->
        let inline convert v = (v <<< 4) + v
        let c = Color.FromRgb ( convert color.Red
                              , convert color.Green
                              , convert color.Blue
                              )
        let b = SolidColorBrush c
        let b = freeze b
        knownBrushes.Add (color, b)

    let polygons =
      f.Polygons
//      |> Array.filter (fun p -> p.Simplified.Length > 0)
//      |> Array.filter (fun p -> not p.IsConvex)
      |> Array.map mapPolygon

    { Polygons = polygons }


  let wpfFrames = 
    frames 
    |> Array.map mapFrame

  { Frames = wpfFrames }

let renderScene (scene : Scene) =
  let wpfScene = toWPFScene scene

  let window  = Window (Title = "ST-NICC in F#", Background = Brushes.Black)
  let element = SceneElement wpfScene
  window.Content            <- element
  element.IsHitTestVisible  <- true
  element.Focusable         <- true
  element.Focus () |> ignore
  element.Start ()
  window.ShowDialog () |> ignore
