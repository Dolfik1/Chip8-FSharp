module Chip8.Operations

module private R =
  let (|NNN|_|) mask opcode =
    if (opcode &&& 0xF000us) = mask then Some (opcode &&& 0x0FFFus) else None
    
  let (|XKK|_|) mask opcode =
    if (opcode &&& 0xF000us) = mask then
      Some (byte ((opcode &&& 0x0F00us) >>> 8), byte (opcode &&& 0x00FFus))
    else
      None
      
  let (|XY|_|) mask opcode =
    if (opcode &&& 0xF00Fus) = mask then
      Some (byte ((opcode &&& 0x0F00us) >>> 8), byte ((opcode &&& 0x00F0us) >>> 4))
    else
      None
      
  let (|XYN|_|) mask opcode =
    if (opcode &&& 0xF000us) = mask then
      Some (byte ((opcode &&& 0x0F00us) >>> 8), byte ((opcode &&& 0x00F0us) >>> 4), byte (opcode &&& 0x000Fus))
    else
      None

  let (|X|_|) mask opcode =
    if (opcode &&& 0xF0FFus) = mask then
      Some (byte ((opcode &&& 0x0F00us) >>> 8))
    else
      None

module private W =
  let NNN mask v = (mask ||| (0x0FFFus &&& v))

  let XKK mask x kk = (mask ||| ((0x0F00us &&& (uint16 x <<< 8)) ||| (0x00FFus &&& uint16 kk)))

  let XY mask x y = (mask ||| ((0x0F00us &&& (uint16 x <<< 8)) ||| (0x00F0us &&& (uint16 y <<< 4))))

  let XYN mask x y n = (mask ||| ((0x0F00us &&& (uint16 x <<< 8)) ||| (0x00F0us &&& (uint16 y <<< 4)) ||| (0x000Fus &&& uint16 n)))

  let X mask x = (mask ||| (0x0F00us &&& (uint16 x <<< 8)))

// http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#3.0
[<Struct>]
type Operations =
  | Cls
  | Ret
  | Sys of addr: uint16
  | Jp of _1_addr: uint16
  | Call of _2_addr: uint16
  | SeVxKK of x: byte * kk: byte
  | SneVxKK of _1_x: byte * _1_kk: byte
  | SeVxVy of _2_x: byte * _1_y: byte
  | LdVxKK of _3_x: byte * _2_kk: byte
  | AddVxKK of _4_x: byte * _3_kk: byte
  | LdVxVy of _5_x: byte * _2_y: byte
  | Or of _6_x: byte * _3_y: byte
  | And of _7_x: byte * _4_y: byte
  | Xor of _8_x: byte * _5_y: byte
  | AddVxVy of _9_x: byte * _6_y: byte
  | Sub of _10_x: byte * _7_y: byte
  | Shr of _11_x: byte * _8_y: byte
  | Subn of _12_x: byte * _9_y: byte
  | Shl of _13_x: byte * _10_y: byte
  | SneVxVy of _14_x: byte * _11_y: byte
  | LdI of _3_addr: uint16
  | JpV0 of _4_addr: uint16
  | Rnd of _15_x: byte * _4_kk: byte
  | Drw of _16_x: byte * _12_y: byte * nibble: byte
  | Skp of _17_x: byte
  | Sknp of _18_x: byte
  | LdVxDT of _19_x: byte
  | LdVxK of _20_x: byte
  | LdDTVx of _21_x: byte
  | LdSTVx of st: byte
  | AddIVx of i: byte
  | LdFVx of f: byte
  | LdBVx of _22_x: byte
  | LdIVx of _1_i: byte
  | LdVxI of _23_x: byte
  | Unknown of opcode: uint16
  static member Parse(opcode: uint16) =
    match opcode with
    | 0x00E0us -> Cls
    | 0x00EEus -> Ret
    | R.NNN 0x0000us v -> Sys v
    | R.NNN 0x1000us v -> Jp v
    | R.NNN 0x2000us v -> Call v
    | R.XKK 0x3000us (x, kk) -> SeVxKK (x, kk)
    | R.XKK 0x4000us (x, kk) -> SneVxKK (x, kk)
    | R.XY 0x5000us (x, y) -> SeVxVy (x, y)
    | R.XKK 0x6000us (x, kk) -> LdVxKK (x, kk)
    | R.XKK 0x7000us (x, kk) -> AddVxKK (x, kk)
    | R.XY 0x8000us (x, y) -> LdVxVy (x, y)
    | R.XY 0x8001us (x, y) -> Or (x, y)
    | R.XY 0x8002us (x, y) -> And (x, y)
    | R.XY 0x8003us (x, y) -> Xor (x, y)
    | R.XY 0x8004us (x, y) -> AddVxVy (x, y)
    | R.XY 0x8005us (x, y) -> Sub (x, y)
    | R.XY 0x8006us (x, y) -> Shr (x, y)
    | R.XY 0x8007us (x, y) -> Subn (x, y)
    | R.XY 0x800Eus (x, y) -> Shl (x, y)
    | R.XY 0x9000us (x, y) -> SneVxVy (x, y)
    | R.NNN 0xA000us v -> LdI v
    | R.NNN 0xB000us v -> JpV0 v
    | R.XKK 0xC000us (x, kk) -> Rnd (x, kk)
    | R.XYN 0xD000us (x, y, n) -> Drw (x, y, n)
    | R.X 0xE09Eus x -> Skp x
    | R.X 0xE0A1us x -> Sknp x
    | R.X 0xF007us x -> LdVxDT x
    | R.X 0xF00Aus x -> LdVxK x
    | R.X 0xF015us x -> LdDTVx x
    | R.X 0xF018us x -> LdSTVx x
    | R.X 0xF01Eus x -> AddIVx x
    | R.X 0xF029us x -> LdFVx x
    | R.X 0xF033us x -> LdBVx x
    | R.X 0xF055us x -> LdIVx x
    | R.X 0xF065us x -> LdVxI x
    | opcode ->
      Unknown opcode
  member x.ToOpcode() =
    match x with
    | Cls -> 0x00E0us
    | Ret -> 0x00EEus
    | Sys addr -> W.NNN 0x0000us addr
    | Jp addr -> W.NNN 0x1000us addr
    | Call addr -> W.NNN 0x2000us addr
    | SeVxKK (x, kk) -> W.XKK 0x3000us x kk
    | SneVxKK (x, kk) -> W.XKK 0x4000us x kk
    | SeVxVy (x, y) -> W.XY 0x5000us x y
    | LdVxKK (x, kk) -> W.XKK 0x6000us x kk
    | AddVxKK (x, kk) -> W.XKK 0x7000us x kk
    | LdVxVy (x, y) -> W.XY 0x8000us x y
    | Or (x, y) -> W.XY 0x8001us x y
    | And (x, y) -> W.XY 0x8002us x y
    | Xor (x, y) -> W.XY 0x8003us x y
    | AddVxVy (x, y) -> W.XY 0x8004us x y
    | Sub (x, y) -> W.XY 0x8005us x y
    | Shr (x, y) -> W.XY 0x8006us x y
    | Subn (x, y) -> W.XY 0x8007us x y
    | Shl (x, y) -> W.XY 0x800Eus x y
    | SneVxVy (x, y) -> W.XY 0x9000us x y
    | LdI addr -> W.NNN 0xA000us addr
    | JpV0 addr -> W.NNN 0xB000us addr
    | Rnd (x, kk) -> W.XKK 0xC000us x kk
    | Drw (x, y, nibble) -> W.XYN 0xD000us x y nibble
    | Skp x -> W.X 0xE09Eus x
    | Sknp x -> W.X 0xE0A1us x
    | LdVxDT x -> W.X 0xF007us x
    | LdVxK x -> W.X 0xF00Aus x
    | LdDTVx x -> W.X 0xF015us x
    | LdSTVx x -> W.X 0xF018us x
    | AddIVx x -> W.X 0xF01Eus x
    | LdFVx x -> W.X 0xF029us x
    | LdBVx x -> W.X 0xF033us x
    | LdIVx x -> W.X 0xF055us x
    | LdVxI x -> W.X 0xF065us x
    | Unknown opcode -> opcode
  override x.ToString() =
    match x with
    | Cls -> "CLS"
    | Ret -> "RET"
    | Sys addr -> $"SYS 0x{addr:X}"
    | Jp addr -> $"JP 0x{addr:X}"
    | Call addr -> $"CALL 0x{addr:X}"
    | SeVxKK (x, kk) -> $"SE V{x}, {kk}"
    | SneVxKK (x, kk) -> $"SNE V{x}, {kk}"
    | SeVxVy (x, y) -> $"SE V{x}, V{y}"
    | LdVxKK (x, kk) -> $"LD V{x}, {kk}"
    | AddVxKK (x, kk) -> $"ADD V{x}, {kk}"
    | LdVxVy (x, y) -> $"LD V{x}, V{y}"
    | Or (x, vy) -> $"OR V{x}, {vy}"
    | And (x, y) -> $"AND V{x}, V{y}"
    | Xor (x, y) -> $"XOR V{x}, V{y}"
    | AddVxVy (x, y) -> $"ADD V{x}, V{y}"
    | Sub (x, y) -> $"SUB V{x}, V{y}"
    | Shr (x, y) -> $"SHR V{x}, V{y}"
    | Subn (x, y) -> $"SUBN V{x}, V{y}"
    | Shl (x, y) -> $"SHL V{x}, V{y}"
    | SneVxVy (x, y) -> $"SNE V{x}, V{y}"
    | LdI addr -> $"LD [I], 0x{addr:X}"
    | JpV0 addr -> $"JP [v0], 0x{addr:X}"
    | Rnd (x, kk) -> $"RND V{x}, {kk}"
    | Drw (x, vy, nibble) -> $"DRW V{x}, {vy}, {nibble}"
    | Skp x -> $"SKP V{x}"
    | Sknp x -> $"SKNP V{x}"
    | LdVxDT x -> $"LD V{x}, DT"
    | LdVxK x -> $"LD V{x}, K"
    | LdDTVx x -> $"LD DT, V{x}"
    | LdSTVx x -> $"LD ST, V{x}"
    | AddIVx x -> $"ADD I, V{x}"
    | LdFVx x -> $"LD F, V{x}"
    | LdBVx x -> $"LD B, V{x}"
    | LdIVx x -> $"LD I, V{x}"
    | LdVxI x -> $"LD V{x}, I"
    | Unknown opcode -> $"UNKNOWN {opcode:X}"