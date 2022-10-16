open System
open System.IO

open Log
open IndentedStreamWriter
open BinaryParser
open SceneParser

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

  WPFRenderer.renderScene scene

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
