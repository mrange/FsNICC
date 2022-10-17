module FsNICC.Spectre
open System
open System.Diagnostics
open System.IO
open System.Threading

open Spectre.Console

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing

open FsNICC
open IndentedStreamWriter
open BinaryParser
open SceneParser

type [<Struct>] SpectrePolygon =
  {
    Fill  : Color
    Path  : PointF array
  }

type [<Struct>] SpectreFrame =
  {
    Polygons  : SpectrePolygon array
  }

type [<Struct>] SpectreScene =
  {
    Frames : SpectreFrame array
  }


let run () =
  Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

  let input   = Path.GetFullPath "scene1.bin"
  let bs = File.ReadAllBytes input
  let scene = BinaryReader.brun SceneReader.bscene bs

  let w = 256
  let h = 200

  let canvas = Canvas (w, h)

  let updater (ctx : LiveDisplayContext) =
    let palette = Array.zeroCreate 16
    use image = new Image<Rgb24> (w, h)
    for frame = 0 to scene.Frames.Length - 1 do
      let inline toColor (rgb : RGB) = 
        let inline convert c = (c <<< 4) + c
        Color.FromRgb (convert rgb.Red, convert rgb.Green, convert rgb.Blue)

      let filler (ctx : IImageProcessingContext) =
        let f = scene.Frames.[frame]
        if f.ClearScreen then
          ctx.Fill (toColor palette.[0]) |> ignore
        for pi in f.PaletteDelta do
          palette.[int pi.ColorIndex.Index] <- pi.Color
        for p in f.Polygons do
          let ps = 
            p.Vertices 
            |> Array.map (fun v -> PointF (float32 v.X, float32 v.Y))
          ctx.FillPolygon (toColor palette.[int p.ColorIndex.Index], ps) |> ignore
      image.Mutate filler

      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let c = image[x, y]
          let sc = Spectre.Console.Color (c.R, c.G, c.B)
          canvas.SetPixel(x, y, sc) |> ignore
      ctx.Refresh()

  let sw = Stopwatch.StartNew ()
  AnsiConsole.Live(canvas).Start(updater)
  sw.Stop ()

  printfn "Rendered %d frames" scene.Frames.Length
  printfn "  it took %0.2f s" (float sw.ElapsedMilliseconds / 1000.)
  printfn "  on average %0.2f FPS" (float scene.Frames.Length/(float sw.ElapsedMilliseconds / 1000.))


  ()
[<EntryPoint>]
let main args =
  run ()
  0