module Chip8.Gen

open System
open System.Text

let ops =
    """00E0 - CLS
00EE - RET
0nnn - SYS addr
1nnn - JP addr
2nnn - CALL addr
3xkk - SE Vx, byte
4xkk - SNE Vx, byte
5xy0 - SE Vx, Vy
6xkk - LD Vx, byte
7xkk - ADD Vx, byte
8xy0 - LD Vx, Vy
8xy1 - OR Vx, Vy
8xy2 - AND Vx, Vy
8xy3 - XOR Vx, Vy
8xy4 - ADD Vx, Vy
8xy5 - SUB Vx, Vy
8xy6 - SHR Vx ,Vy
8xy7 - SUBN Vx, Vy
8xyE - SHL Vx, Vy
9xy0 - SNE Vx, Vy
Annn - LD I, addr
Bnnn - JP V0, addr
Cxkk - RND Vx, byte
Dxyn - DRW Vx, Vy, nibble
Ex9E - SKP Vx
ExA1 - SKNP Vx
Fx07 - LD Vx, DT
Fx0A - LD Vx, K
Fx15 - LD DT, Vx
Fx18 - LD ST, Vx
Fx1E - ADD I, Vx
Fx29 - LD F, Vx
Fx33 - LD B, Vx
Fx55 - LD [I], Vx
Fx65 - LD Vx, [I]"""

type OpType =
  | NNN
  | XKK
  | XYN
  | XY
  | X
  | Exact

let pascalCase (s: string) =
  String(s |> Seq.mapi (fun i x -> if i = 0 then Char.ToUpper(x) else Char.ToLower(x)) |> Array.ofSeq)

let gen () =
  let ops =
    ops.Split("\n")
    |> Array.map (fun x ->
      let opcode = x.Substring(0, 4)
      let opName = x.Substring(7).Split(" ")
      {|
        Opcode = opcode
        OpName = opName[0]
        OpArgs = opName |> Array.skip 1 |> Array.map (fun x -> x.Replace(",", ""))
        Original = x
      |})

  let opsGrouped =
    ops
    |> Array.groupBy (fun x -> x.OpName)
    |> Map.ofArray
  
  let cases =
    ops
    |> Array.map (fun x ->
      let args =
        x.OpArgs
        |> Array.map (fun x -> x.Replace("byte", "KK").Replace("[", "").Replace("]", ""))
      
      let argsLower = args |> Array.map (fun x -> x.ToLowerInvariant())
      
      let caseName =
        let items = opsGrouped |> Map.find x.OpName
        let op = pascalCase x.OpName
        if items.Length = 1 then pascalCase op
        else
          let s = String.Join("", args)
          $"{op}{s}"
      {|
        CaseName = caseName
        OpType =
          if x.Opcode.Contains("nnn") then OpType.NNN
          elif x.Opcode.Contains("xkk") then OpType.XKK
          elif x.Opcode.Contains("xyn") then OpType.XYN
          elif x.Opcode.Contains("xy") then OpType.XY
          elif x.Opcode.Contains("x") then OpType.X
          else OpType.Exact
        Args = argsLower
        Original = x.Original
        OpNameOriginal = x.OpName
        Opcode = x.Opcode
      |}
    )
  
  printfn "type Operations ="
  let s = StringBuilder()
  for case in cases do
    s.Clear().Append($"  | {case.CaseName}") |> ignore
    match case.OpType with
    | OpType.Exact -> ()
    | OpType.NNN ->
      s.Append(" of ").Append(case.Args[0]).Append(": uint16") |> ignore
    | OpType.XKK ->
      s.Append(" of ").Append(case.Args[0]).Append(": byte").Append(" * ").Append(case.Args[1]).Append(": byte") |> ignore
    | OpType.XYN ->
      s.Append(" of ").Append(case.Args[0]).Append(": byte").Append(" * ").Append(case.Args[1]).Append(": byte * ").Append(case.Args[2]).Append(": byte") |> ignore
    | OpType.XY ->
      s.Append(" of ").Append(case.Args[0]).Append(": byte").Append(" * ").Append(case.Args[1]).Append(": byte") |> ignore
    | OpType.X ->
      s.Append(" of ").Append(case.Args[0]).Append(": byte") |> ignore
    
    printfn $"{s}"
  
  printfn "  static member Parse(opcode: uint16) ="
  printfn "    match opcode with"
  for case in cases do
    match case.OpType with
    | Exact ->
      printfn $"    | 0x{case.Opcode}us -> {case.CaseName}"
    | NNN ->
      let oc = case.Opcode.Replace("nnn", "000")
      printfn $"    | NNN 0x{oc}us v -> {case.CaseName} v"
    | XKK ->
      let oc = case.Opcode.Replace("xkk", "000")
      printfn $"    | XKK 0x{oc}us (x, kk) -> {case.CaseName} (x, kk)"
    | XYN ->
      let oc = case.Opcode.Replace("xyn", "000")
      printfn $"    | XYN 0x{oc}us (x, y, n) -> {case.CaseName} (x, y, n)"
    | XY ->
      let oc = case.Opcode.Replace("xy", "00")
      printfn $"    | XY 0x{oc}us (x, y) -> {case.CaseName} (x, y)"
    | X ->
      let oc = case.Opcode.Replace("x", "0")
      printfn $"    | X 0x{oc}us x -> {case.CaseName} x"
    
  
  printfn "  override x.ToString() ="
  printfn "    match x with"
  for case in cases do
    s.Clear().Append($"    | {case.CaseName}") |> ignore
    
    if case.Args.Length = 0 then
      s.Append($" -> \"{case.OpNameOriginal}\"") |> ignore
    elif case.Args.Length = 1 then
      s.Append($" {case.Args[0]} -> $\"{case.OpNameOriginal} {{{case.Args[0]}}}\"") |> ignore
    else
      s.Append(" (") |> ignore
      for arg in case.Args do
        s.Append(arg) |> ignore
        if arg <> (case.Args |> Array.last) then
          s.Append(", ") |> ignore
      
      s.Append(") -> ") |> ignore
      s.Append($"$\"{case.OpNameOriginal} ") |> ignore
      for arg in case.Args do
        s.Append("{").Append(arg).Append("}") |> ignore
        if arg <> (case.Args |> Array.last) then
          s.Append(", ") |> ignore
      s.Append("\"") |> ignore
      
    printfn $"{s}"
  ()