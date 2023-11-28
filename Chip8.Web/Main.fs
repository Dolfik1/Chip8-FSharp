module Chip8.Web.Main

open System
open System.Net.Http
open System.Timers
open Chip8
open Elmish
open Bolero
open Bolero.Html
open Excubo.Blazor.Canvas
open Microsoft.AspNetCore.Components.Web

[<Literal>]
let PixelSize = 10.0

type Chip8Canvas() =
  inherit Canvas()
  
  let mutable memory: Memory<uint8> = Memory.Empty
  let pressedKeys = Array.zeroCreate 16
  let mutable timer: Timer = Unchecked.defaultof<Timer>

  override x.OnInitializedAsync() =
    task {
      use client = new HttpClient()
      
      let name = "Demos/Trip8 Demo (2008) [Revival Studios].ch8"
      //let name = "Keypad Test [Hap, 2006].ch8"
      //let name = "Games/Space Invaders [David Winter].ch8"
      let! rom = client.GetByteArrayAsync($"http://localhost:5000/ROMs/{name}")
      
      timer <-
        Cpu.create x
        |> Cpu.loadBytes rom
        |> Cpu.runWithTimer
    }
  
  override x.OnAfterRenderAsync _ =
    task {
      use! ctx = x.GetContext2DAsync()
      do! ctx.ClearRectAsync(0, 0, float Cpu.DisplayWidth * PixelSize, float Cpu.DisplayHeight * PixelSize)
      do! ctx.FillStyleAsync("limegreen")
      
      for i in 0 .. memory.Span.Length - 1 do
        let pixelValue = memory.Span[i]
        if pixelValue <> 0uy then
          let x = i % Cpu.DisplayWidth
          let y = int <| Math.Floor(float i / float Cpu.DisplayWidth)
          
          do! ctx.FillRectAsync(float (float x * PixelSize), float (float y * PixelSize), float PixelSize, float PixelSize)
    }
  
  member x.OnKeyUp(args: KeyboardEventArgs) =
    pressedKeys[0] <- 0uy
    ()

  member x.OnKeyDown(args: KeyboardEventArgs) =
    pressedKeys[0] <- 1uy
    ()
    
  interface Cpu.IDevice with
    member x.CheckKeyboard(keys) =
      for i in 0 .. pressedKeys.Length do
        keys[i] <- pressedKeys[0]
      
    member x.FlushDisplay(displayMemory) =
      memory <- displayMemory
      x.StateHasChanged()

    member x.UpdateSound _ =
      ()

type Model = {
  CanvasRef: Ref<Chip8Canvas>
}

type Message =
  | KeyUp of KeyboardEventArgs
  | KeyDown of KeyboardEventArgs

let update msg (model: Model) =
  match msg with
  | KeyUp args ->
    model.CanvasRef.Value |> Option.iter (fun x -> x.OnKeyUp args)
  | KeyDown args ->
    model.CanvasRef.Value |> Option.iter (fun x -> x.OnKeyDown args)
  model, Cmd.none

let view model dispatch =
  body {
    on.keyup (fun (args: KeyboardEventArgs) -> dispatch (KeyUp args)) 
    on.keydown (fun (args: KeyboardEventArgs) -> dispatch (KeyDown args))
    comp<Chip8Canvas> {
      attr.width $"{float Cpu.DisplayWidth * PixelSize}px"
      attr.height $"{float Cpu.DisplayHeight * PixelSize}px"
      
      model.CanvasRef
    }
  }

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override this.Program =
    Program.mkProgram (fun _ -> { CanvasRef = Ref<_>() }, Cmd.none) update view