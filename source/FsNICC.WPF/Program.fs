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
  let bs = File.ReadAllBytes input
  let scene = BinaryReader.brun SceneReader.bscene bs

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
