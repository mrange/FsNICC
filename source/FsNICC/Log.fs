module Log

open System
open FSharp.Core.Printf

let log cc msg =
  let bcc = Console.ForegroundColor
  Console.ForegroundColor <- cc
  try
    Console.WriteLine (msg : string)
  finally
    Console.ForegroundColor <- bcc

let good msg = log ConsoleColor.Green   msg
let hili msg = log ConsoleColor.Cyan    msg
let info msg = log ConsoleColor.Gray    msg
let warn msg = log ConsoleColor.Yellow  msg
let fail msg = log ConsoleColor.Red     msg

let goodf fmt = kprintf good fmt
let hilif fmt = kprintf hili fmt
let infof fmt = kprintf info fmt
let warnf fmt = kprintf warn fmt
let failf fmt = kprintf fail fmt


