module Chip8.MonoGame.Chip8Emulator

open System
open Chip8
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Cpu8Emulator () as x =
  inherit Game()
  
  [<Literal>]
  let DebugMode = false
  
  let PixelColor = Color.LimeGreen
  let mutable displayMemory: Memory<uint8> = Memory.Empty
 
  do x.Content.RootDirectory <- "Chip-8"

  let graphics = new GraphicsDeviceManager(x)
  do graphics.PreferredBackBufferWidth <- Cpu.DisplayWidth * 10
  do graphics.PreferredBackBufferHeight <- Cpu.DisplayHeight * 10
  do graphics.ApplyChanges()
  
  let mutable pixel: Texture2D = null
  let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

  let sampleRate = 44100
  let channels = 1
  
  let buffer =
    let frequency = 600.0
    let amplitude = 0.25
    let mutable sample = 0.0
    let increment = frequency * 2.0 * Math.PI / float sampleRate

    let buffer = Array.zeroCreate<byte> (sampleRate * channels * 2)
    for i in 0 .. buffer.Length / 2 - 1 do
        let value = Convert.ToInt16(Math.Sin(sample) * amplitude * float Int16.MaxValue)
        buffer[i * 2] <- byte (value &&& 0xffs)
        buffer[i * 2 + 1] <- byte (value >>> 8)
        sample <- sample + increment
    buffer
    
  let soundEffect = new DynamicSoundEffectInstance(44100, enum<AudioChannels> channels)
  do soundEffect.BufferNeeded.Add(fun _ -> soundEffect.SubmitBuffer(buffer))
  
  let mutable state =
    Cpu.create x |> Cpu.loadOps (Intro.ops())
  
  override x.Initialize() =  
    spriteBatch <- new SpriteBatch(x.GraphicsDevice)
    
    base.Initialize()

    x.Window.FileDrop.Add(x.FileDropped)
    x.Window.Title <- "Chip-8 Emulator"

  override x.LoadContent() =
    pixel <- new Texture2D(x.GraphicsDevice, 10, 10)
    pixel.SetData(Array.create (10 * 10) PixelColor)

  override x.Update _ =
    let ks = Keyboard.GetState()
    if (ks.IsKeyDown(Keys.LeftControl) || ks.IsKeyDown(Keys.RightControl)) && ks.IsKeyDown(Keys.C) then
      state <- Cpu.create x |> Cpu.loadOps (Intro.ops())
    elif not DebugMode then
      state <- Cpu.stepN 16 state
    else
      if Keyboard.GetState().IsKeyDown(Keys.Space) then
        state <- Cpu.stepN 16 state

  member x.FileDropped(args: FileDropEventArgs) =
    if args.Files.Length > 0 then
      state <- Cpu.create x |> Cpu.loadRom args.Files[0]
  
  override x.Draw(gameTime) =
    let displayData = displayMemory.Span
    let pixelSize = 10 // TODO move to const
    x.GraphicsDevice.Clear(Color.Black)
    spriteBatch.Begin()
    for i in 0 .. displayData.Length - 1 do
      let pixelValue = displayData[i]
      if pixelValue <> 0uy then
        let x = i % Cpu.DisplayWidth
        let y = int <| Math.Floor(float i / float Cpu.DisplayWidth)
        
        let position = Vector2(float32 (x * pixelSize), float32 (y * pixelSize))
        spriteBatch.Draw(pixel, position, PixelColor)

    spriteBatch.End()

    base.Draw(gameTime)
  
  interface Cpu.IDevice with
    member x.FlushDisplay(memory: Memory<uint8>) =
      displayMemory <- memory
    
    member x.UpdateSound(beep: bool) =
      if beep
      then soundEffect.Play()
      else soundEffect.Stop()
    
    member x.CheckKeyboard(keypads) =
      let state = Keyboard.GetState()
      let inline chk k = if state.IsKeyDown(k) then 1uy else 0uy
      keypads[0] <- chk Keys.D0
      keypads[1] <- chk Keys.D1
      keypads[2] <- chk Keys.D2
      keypads[3] <- chk Keys.D3
      keypads[4] <- chk Keys.D4
      keypads[5] <- chk Keys.D5
      keypads[6] <- chk Keys.D6
      keypads[7] <- chk Keys.D7
      keypads[8] <- chk Keys.D8
      keypads[9] <- chk Keys.D9
      keypads[10] <- chk Keys.A
      keypads[11] <- chk Keys.B
      keypads[12] <- chk Keys.C
      keypads[13] <- chk Keys.D
      keypads[14] <- chk Keys.E
      keypads[15] <- chk Keys.F