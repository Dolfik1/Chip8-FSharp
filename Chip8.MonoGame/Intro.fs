module Chip8.MonoGame.Intro

open System
open System.Buffers.Binary
open Chip8
open Chip8.Operations
open Chip8.Asm

let private bytesToUInt16 (bytes: byte[]) =
  let bytes = Span.op_Implicit(bytes)
  let length = bytes.Length
  let uint16Array = Array.zeroCreate<uint16> (length / 2)
  for i in 0 .. 2 .. length - 1 do
    uint16Array[i / 2] <- BinaryPrimitives.ReadUInt16BigEndian(bytes.Slice(i, 2))

  uint16Array

let private fontSet () =
  [|
    0x28uy; 0x7Cuy; 0x28uy; 0x7Cuy; 0x28uy // #
    0x00uy; 0x00uy; 0x3Cuy; 0x00uy; 0x00uy // -
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy // 8
    0xF0uy; 0x80uy; 0x80uy; 0x80uy; 0xF0uy // C
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0xF0uy // D
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0x80uy // F
    0xF0uy; 0x80uy; 0xF0uy; 0x10uy; 0xF0uy // H
    0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy // I
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0x90uy // M
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy // O
    0xF0uy; 0x90uy; 0xF0uy; 0x10uy; 0x10uy // P
    0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy // R
    0xF0uy; 0x10uy; 0x10uy; 0x10uy; 0xF0uy // T
    0xF0uy; 0x10uy; 0xF0uy; 0x10uy; 0xF0uy // W
  |] |> bytesToUInt16 |> Array.map Operations.Parse

let private screen () =
  """
                                                                
     X X                                                        
    XX XX                                                       
   XX   XX    XX  XX  XXX XXX   XX  XXX X X   X X XXX XX  XXX   
  XX X   XX   X X X X X X X X   X X X X XXX   X X X   X X X     
 XX XX    XX  X X XX  X X XXX   XX  X X X X   XXX XX  XX  XX    
  XX X   XX   X X X X X X X     X X X X X X   X X X   X X X     
   XX   XX    XX  X X XXX X     X X XXX X X   X X XXX X X XXX   
    XX XX                                                       
     X X                                                        
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
        XXX XXX XX  X       XXX        X  XX  XXX XX  XXX       
        X    X  X X X    X  X         X X X X X X X X  X        
        X    X  XX  X   XXX X    XXX  XXX XX  X X XX   X        
        X    X  X X X    X  X         X X X X X X X X  X        
        XXX  X  X X XXX     XXX       X X XX  XXX X X  X        
                                                                
                                                                
                                                                
                                                                
                                                                
""", Cpu.DisplayWidth, Cpu.DisplayHeight

let private screen2 () =
  """
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 
""", 8, 8

let ops () =
  let screen, sw, sh = (screen ())
  let screen = screen.Replace("\n", "")

  if screen.Length <> sw * sh then
    failwith "Wrong screen size!"
  
  let data =
    [|
      for y in 0 .. 8 .. sh - 1 do
        for x in 0 .. 8 .. sw - 1 do
          for i in y .. min (y + 7) (sh - 1) do
            let mutable b = 0x0uy
            for j in x .. min (x + 7) (sw - 1) do
              let c = screen[i * sw + j]
              match c with
              | 'X' -> b <- b ||| (1uy <<< (7 - j % 8))
              | ' ' -> ()
              | _ -> failwith $"Wrong character: {c}"
            yield b
    |] |> bytesToUInt16 |> Array.map Operations.Parse

  [
    Jp (ProgramOffset + OpLen + uint16 data.Length * OpLen)
    yield! data
    
    let mutable addr = ProgramOffset + OpLen
    for y in 0 .. (sh / 8) - 1 do
      for x in 0 .. (sw / 8) - 1 do
        LdI addr
        LdVxKK (R0, uint8 (x * 8))
        LdVxKK (R1, uint8 (y * 8))
        Drw (R0, R1, 0x8uy)
        addr <- addr + (OpLen * 4us)
    
    LdVxK 0x0uy // wait for key
    Jp 0x200us // restart
  ]