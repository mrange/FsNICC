(*
Copyright 2022 Mårten Rånge

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
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