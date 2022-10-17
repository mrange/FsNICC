module FsNICC.WPF.Program
open System
open System.IO
open System.Windows

open FsNICC
open IndentedStreamWriter
open BinaryParser
open SceneParser

let run () =
  Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
  let input   = Path.GetFullPath "scene1.bin"
  let output  = Path.GetFullPath "../../../../../assets/scene1.txt"

  let bs = File.ReadAllBytes input

  let scene = BinaryReader.brun SceneReader.bscene bs

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
    let msg = sprintf "Caught exception: %s" e.Message
    MessageBox.Show ( msg
                    , "Application crashed"
                    , MessageBoxButton.OK
                    , MessageBoxImage.Error
                    ) |> ignore
    
    99
