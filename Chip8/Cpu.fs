[<RequireQualifiedAccess>]
module Chip8.Cpu

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.IO
open System.Timers
open Chip8.Operations


[<Literal>]
let DisplayWidth = 64

[<Literal>]
let DisplayHeight = 32

[<Literal>]
let FontsetAddress = 0x50us

type IDevice =
  abstract member FlushDisplay: display: Memory<uint8> -> unit
  abstract member UpdateSound: beep: bool -> unit
  abstract member CheckKeyboard: uint8[] -> unit

type State =
  {
    CurrentUnsupported: bool
    Display: Memory<uint8>
    Memory: Memory<uint8>
    VRegister: uint8[]
    IndexRegister: uint16
    ProgramCounter: uint16
    DelayTimer: uint8
    SoundTimer: uint8
    Stack: Stack<uint16>
    StackPointer: uint16
    KeypadState: uint8[]
    Device: IDevice
    WaitingForKeyPress: bool
  }
  static member Default device =
    {
      CurrentUnsupported = false
      Display = Memory(Array.zeroCreate (DisplayWidth * DisplayHeight))
      Memory = Memory(Array.zeroCreate 4096)
      VRegister = Array.zeroCreate 16
      IndexRegister = 0us
      ProgramCounter = 0x200us
      DelayTimer = 0uy
      SoundTimer = 0uy
      Stack = Stack<uint16>(16)
      StackPointer = 0us
      KeypadState = Array.zeroCreate 16
      Device = device
      WaitingForKeyPress = false
    }

let private fontSet () =
  [|
    0xF0uy; 0x90uy; 0x90uy; 0x90uy; 0xF0uy; // 0
    0x20uy; 0x60uy; 0x20uy; 0x20uy; 0x70uy; // 1
    0xF0uy; 0x10uy; 0xF0uy; 0x80uy; 0xF0uy; // 2
    0xF0uy; 0x10uy; 0xF0uy; 0x10uy; 0xF0uy; // 3
    0x90uy; 0x90uy; 0xF0uy; 0x10uy; 0x10uy; // 4
    0xF0uy; 0x80uy; 0xF0uy; 0x10uy; 0xF0uy; // 5
    0xF0uy; 0x80uy; 0xF0uy; 0x90uy; 0xF0uy; // 6
    0xF0uy; 0x10uy; 0x20uy; 0x40uy; 0x40uy; // 7
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy; // 8
    0xF0uy; 0x90uy; 0xF0uy; 0x10uy; 0xF0uy; // 9
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0x90uy; // A
    0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy; // B
    0xF0uy; 0x80uy; 0x80uy; 0x80uy; 0xF0uy; // C
    0xE0uy; 0x90uy; 0x90uy; 0x90uy; 0xE0uy; // D
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0xF0uy; // E
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0x80uy  // F
  |]

let private loadFontSet state =
  let fontSet = fontSet ()
  let slice = state.Memory.Slice(int FontsetAddress, fontSet.Length)
  Span(fontSet).CopyTo(slice.Span)
  state

let private put i v state =
  state.VRegister[int i] <- v
  state

let private next state =
  { state with ProgramCounter = state.ProgramCounter + 2us }

let private skip cond state =
  if cond then
    state |> next
  else
    state

let updateTimers (state: State) =
  let dt = if state.DelayTimer > 0uy then state.DelayTimer - 1uy else state.DelayTimer
  let st = if state.SoundTimer > 0uy then state.SoundTimer - 1uy else state.SoundTimer
  { state with DelayTimer = dt; SoundTimer = st }
    

let private (+.) (x: byte) (y: byte) = Checked.(+) x y
let private (-.) (x: byte) (y: byte) = Checked.(-) x y

let create device =
  let defaultState = State.Default device
  defaultState
  |> loadFontSet

let loadRom path state =
  let rom = File.ReadAllBytes(path)
  let slice = state.Memory.Slice(0x200, rom.Length)
  Span(rom).CopyTo(slice.Span)
  state

let printRomInstructions path =
  let rom = Span(File.ReadAllBytes(path))
  let mutable counter = 0
  while counter < rom.Length do
    let opcode = BinaryPrimitives.ReadUInt16BigEndian(rom.Slice(int counter, 2))
    printf $"{opcode:X}  "
    printfn $"{Operations.Parse(opcode).ToString()}"
    counter <- counter + 2

let load (rom: uint16[]) state =
  let rom =
    rom
    |> Array.map (fun x -> BitConverter.GetBytes(x) |> Array.rev)
    |> Array.concat

  let slice = state.Memory.Slice(0x200, rom.Length)
  Span(rom).CopyTo(slice.Span)
  state

let loadBytes (rom: uint8[]) state =
  let slice = state.Memory.Slice(0x200, rom.Length)
  Span(rom).CopyTo(slice.Span)
  state

let loadOps (ops: Operations.Operations seq) =
  ops
  |> Seq.collect (fun x ->
    let r = x.ToOpcode()
    [| byte (r >>> 8); byte r |])
  |> Array.ofSeq
  |> loadBytes

let setI i state =
  { state with IndexRegister = i }

let putV x value state =
  state.VRegister[x] <- value
  state

let step state =
  let opcode =
    BinaryPrimitives.ReadUInt16BigEndian(state.Memory.Slice(int state.ProgramCounter, 2).Span)

  let op = Operations.Parse opcode
  let state =
    match op with
    | LdVxK _ -> state
    | _ -> next state

  //printf $"I = 0x{state.IndexRegister:X}, SP = 0x{state.StackPointer:X}"
  //let p = String.Join(", ", state.VRegister |> Array.mapi (fun i x -> $"V{i} = 0x{x:X}"))
  //printfn $", {p}"
  //printfn ""
  //printfn $"0x{opcode:X} {op}, pc = 0x{state.ProgramCounter:X}"
  
  match op with
  | Cls ->
    for i in 0 .. state.Display.Span.Length - 1 do
      state.Display.Span[i] <- 0uy
    state
  
  | Rnd (x, kk) ->
    let r = byte <| Random.Shared.Next(0, 255)
    state.VRegister[int x] <- r &&& kk 
    state
  
  | Jp addr ->
    { state with ProgramCounter = addr }

  | LdVxKK (x, kk) ->
    state |> put x kk

  | LdVxVy (x, y) ->
    state.VRegister[int x] <- state.VRegister[int y]
    state

  | LdIVx x ->
    for i in 0 .. int x do
      state.Memory.Span[int state.IndexRegister + i] <- state.VRegister[i]
    
    state

  | LdVxI x ->
    for i in 0 .. int x do
      state.VRegister[i] <- state.Memory.Span[int state.IndexRegister + i]
    
    state
    
  | Sys _ ->
    state
  
  | Call addr ->
    let state = { state with StackPointer = state.StackPointer + 1us }
    state.Stack.Push(state.ProgramCounter)
    { state with ProgramCounter = addr }
  
  | LdI addr ->
    state |> setI addr
  
  | LdBVx x ->
    let vx = state.VRegister[int x]
    let mem = state.Memory.Span.Slice(int state.IndexRegister, 3)
    
    mem[0] <- vx / 100uy
    mem[1] <- (vx % 100uy) / 10uy
    mem[2] <- vx % 10uy
    
    state
  
  | LdFVx x ->
    // Set I = location of sprite for digit Vx.
    // The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx. See section 2.4, Display, for more information on the Chip-8 hexadecimal font.

    let digit = state.VRegister[int x]
    let i = FontsetAddress + uint16 (digit * 5uy)
    state |> setI i
  
  | Drw (x, y, n) ->
    let mutable flag = false
    let vX = state.VRegister[int x]
    let vY = state.VRegister[int y]
    for row in 0 .. int n - 1 do
      let spriteByte = state.Memory.Span[int state.IndexRegister + row]
      for col in 0 .. 7 do
        let displaySpan = state.Display.Span
        let pixelX = (int vX + col) % DisplayWidth
        let pixelY = (int vY + row) % DisplayHeight
        let pixelIndex = pixelY * DisplayWidth + pixelX
        let v = ((spriteByte >>> (7 - col)) &&& 0x1uy)
        displaySpan[pixelIndex] <- displaySpan[pixelIndex] ^^^ byte v
        if displaySpan[pixelIndex] = 0uy && v = 1uy then
          flag <- true

    
    state.VRegister[0xF] <- if flag then 1uy else 0uy
            
    state
  
  | SeVxKK (x, kk) ->
    state |> skip (state.VRegister[int x] = kk)

  | SneVxKK (x, kk) ->
    state |> skip (state.VRegister[int x] <> kk)
  
  | SneVxVy (x, y) ->
    state |> skip (state.VRegister[int x] <> state.VRegister[int y])

  | SeVxVy (x, y) ->
    state |> skip (state.VRegister[int x] = state.VRegister[int y])
  
  | AddVxKK (x, kk) ->
    if kk > (0xFFuy - state.VRegister[int x])
    then state.VRegister[0xF] <- 1uy
    else state.VRegister[0xF] <- 0uy

    state.VRegister[int x] <- state.VRegister[int x] + kk
    state

  | AddIVx x ->
    { state with IndexRegister = state.IndexRegister + uint16 state.VRegister[int x] }
  
  | AddVxVy (x, y) ->
    let r = uint16 state.VRegister[int x] + uint16 state.VRegister[int y]
    state.VRegister[int x] <- uint8 r
    state.VRegister[0xF] <- uint8 (r >>> 8)
    
    state

  | Sub (x, y) ->
    let r = uint16 state.VRegister[int x] - uint16 state.VRegister[int y]
    state.VRegister[int x] <- uint8 r
    state.VRegister[0xF] <- if r > 255us then 0uy else 1uy
    state
  
  | Shr (x, _) ->
    let carry = state.VRegister[int x] &&& 1uy
    state.VRegister[int x] <- state.VRegister[int x] >>> 1
    state.VRegister[0xF] <- carry
    state
  
  | Shl (x, _) ->
    let carry = state.VRegister[int x] >>> 7
    state.VRegister[int x] <- state.VRegister[int x] <<< 0x1
    state.VRegister[0xF] <- carry
    state
  
  | Ret ->
    let addr = state.Stack.Pop()
    { state with ProgramCounter = addr; StackPointer = state.StackPointer - 1us }
  
  | Or (x, y) ->
    state.VRegister[int x] <- state.VRegister[int x] ||| state.VRegister[int y]
    state
  
  | And (x, y) ->
    state.VRegister[int x] <- state.VRegister[int x] &&& state.VRegister[int y]
    state
  
  | Xor (x, y) ->
    state.VRegister[int x] <- state.VRegister[int x] ^^^ state.VRegister[int y]
    state
  
  | Subn (x, y) ->
    // 8xy7 - SUBN Vx, Vy
    let r = uint16 (state.VRegister[int y]) - uint16 (state.VRegister[int x])
    state.VRegister[int x] <- uint8 r
    state.VRegister[0xF] <- if r > 255us then 0uy else 1uy 
    
    state
  
  | JpV0 addr ->
    { state with ProgramCounter = addr + uint16 state.VRegister[0] }
  
  | LdVxK x ->
    state.Device.CheckKeyboard(state.KeypadState)
    let pressed = state.KeypadState |> Array.tryFindIndex (fun x -> x = 1uy)
    match pressed with
    | Some pressedKey ->
      state.VRegister[int x] <- uint8 pressedKey
      { state with WaitingForKeyPress = false } |> next
    | None ->
      { state with WaitingForKeyPress = true }

  | LdDTVx x ->
    { state with DelayTimer = state.VRegister[int x] }
  
  | LdVxDT x ->
    state.VRegister[int x] <- state.DelayTimer
    state
    
  | LdSTVx x ->
    { state with SoundTimer = state.VRegister[int x] }
  
  | Sknp x ->
    // Skip next instruction if key with the value of Vx is not pressed.
    state.Device.CheckKeyboard state.KeypadState
    state |> skip (state.KeypadState[int state.VRegister[int x]] = 0uy)

  | Skp x ->
    // Skip next instruction if key with the value of Vx is pressed.
    state.Device.CheckKeyboard state.KeypadState
    state |> skip (state.KeypadState[int state.VRegister[int x]] <> 0uy)
  
  | Unknown op ->
    printfn $"Unsupported operation! {op}"
    { state with CurrentUnsupported = true }
    
let stepN n state =
  let rec loop n state =
    if n >= 0 && not state.WaitingForKeyPress then
      step state |> loop (n - 1)
    else
      { state with WaitingForKeyPress = false }
  
  let state = loop n state |> updateTimers
  state.Device.UpdateSound(state.SoundTimer <> 0uy)
  state.Device.FlushDisplay state.Display
  state

let runAsync v =
  let rec loop state =
    async {
      do! Async.Sleep 16
      return! state |> stepN 16 |> loop
    }
  loop v
 
let runWithTimer v =
  let timer = new Timer(TimeSpan.FromMilliseconds 16)
  timer.AutoReset <- true
  let mutable state = v
  timer.Elapsed.Add(fun _ ->
    state <- stepN 16 state)
  timer.Start()
  timer