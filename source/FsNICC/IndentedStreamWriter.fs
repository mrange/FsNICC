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
module FsNICC.IndentedStreamWriter
open System
open System.IO
open FSharp.Core.Printf

type IndentContext =
  {
    IndentBy  : int
  }

type 'T IndentedOutput = IndentContext -> StreamWriter -> int -> 'T
module IndentedOutput =
  let irun indentBy sw (io : 'T IndentedOutput) =
    let ctx = {IndentBy = indentBy}
    io ctx sw 0

  let inline ivalue v : 'T IndentedOutput = fun ctx sw i ->
    v

  let inline iindent
    ([<InlineIfLambda>] io  : 'T IndentedOutput )
    : 'T IndentedOutput = fun ctx sw i ->
    io ctx sw (i + ctx.IndentBy)

  let inline iline (msg : string) : unit IndentedOutput = fun ctx sw i ->
    sw.WriteLine ()
    if i > 0 then
      sw.Write (new String (' ', i))
    sw.Write msg

  let inline ilinef fmt = kprintf iline fmt

  let inline iiter
    (vs                    : 'T seq                     )
    ([<InlineIfLambda>] uf : 'T -> unit IndentedOutput  )
    : unit IndentedOutput = fun ctx sw i ->
    for v in vs do
        uf v ctx sw i

  type IndentedOutputBuilder () =
    class
      member inline x.Bind  ( [<InlineIfLambda>] t  : _ IndentedOutput
                            , [<InlineIfLambda>] uf : _ -> _ IndentedOutput
                            ) : _ IndentedOutput = fun ctx sw i ->
        uf (t ctx sw i) ctx sw i

      member inline x.Combine ( [<InlineIfLambda>] t : _ IndentedOutput
                              , [<InlineIfLambda>] u : _ IndentedOutput
                              ) : _ IndentedOutput =  fun ctx sw i ->
        t ctx sw i
        u ctx sw i

      member inline x.MergeSources  ( [<InlineIfLambda>] t : _ IndentedOutput
                                    , [<InlineIfLambda>] u : _ IndentedOutput
                              ) : _ IndentedOutput =  fun ctx sw i ->
        let tv = t ctx sw i
        let uv = u ctx sw i
        struct (tv, uv)

      member inline x.BindReturn    ( [<InlineIfLambda>] t : _ IndentedOutput
                                    , [<InlineIfLambda>] f
                              ) : _ IndentedOutput =  fun ctx sw i ->
        let tv = t ctx sw i
        f tv

      member inline x.For     ( vs                    : 'T seq
                              , [<InlineIfLambda>] uf : 'T -> unit IndentedOutput
                              ) : unit IndentedOutput =  fun ctx sw i ->
        iiter vs uf ctx sw i

      member inline x.Return  ( v : 'T
                              ) : 'T IndentedOutput =
        ivalue v
      member inline x.ReturnFrom  (io : 'T IndentedOutput
                                  ): 'T IndentedOutput =
        io
    end

  let ioutput = IndentedOutputBuilder ()

