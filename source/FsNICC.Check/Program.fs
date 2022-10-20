module FsNICC.Check
open System
open System.IO
open System.Threading

open FsNICC
open IndentedStreamWriter
open BinaryParser
open SceneParser

let run () =
  Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

  let input   = Path.GetFullPath "scene1.bin"

  printfn "Reading scene: %s" input
  let bs      = File.ReadAllBytes input

  printfn "Parsing scene: %s" input
  let scene   = BinaryReader.brun SceneReader.bscene bs

  let output  = Path.GetFullPath "../../../../../assets/scene1.txt"

  printfn "Writing scene: %s" output
  use sw      = File.CreateText output
  IndentedOutput.irun 2 sw (SceneWriter.iwriteScene scene)

[<EntryPoint>]
let main args =
  run ()
  0