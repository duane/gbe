pub const ROM: u16 = 0x0000;
pub const ROM_END: u16 = 0x3FFF;
pub const ROM_SIZE: usize = (ROM_END - ROM + 1) as usize;

pub const ROMX: u16 = 0x4000; // switchable ROM bank
pub const ROMX_END: u16 = 0x7FFF;
pub const ROMX_SIZE: usize = (ROMX_END - ROMX + 1) as usize;

pub const VRAM: u16 = 0x8000;
pub const VRAM_END: u16 = SCRN0 - 1;
pub const VRAM_SIZE: usize = (VRAM_END - VRAM + 1) as usize;

pub const SCRN0: u16 = 0x9800;
pub const SCRN0_END: u16 = 0x9BFF;
pub const SCRN0_SIZE: usize = (SCRN0_END - SCRN0 + 1) as usize;

pub const SCRN1: u16 = 0x9C00;
pub const SCRN1_END: u16 = 0x9FFF;
pub const SCRN1_SIZE: usize = (SCRN1_END - SCRN1 + 1) as usize;

pub const SRAM: u16 = 0xA000;
pub const SRAM_END: u16 = 0xBFFF;
pub const SRAM_SIZE: usize = (SRAM_END - SRAM + 1) as usize;

pub const RAM: u16 = 0xC000;
pub const RAM_END: u16 = 0xCFFF;
pub const RAM_SIZE: usize = (RAM_END - RAM + 1) as usize;

pub const RAMBANK: u16 = 0xD000;
pub const RAMBANK_END: u16 = ECHORAM - 1;
pub const RAMBANK_SIZE: usize = (RAMBANK_END - RAMBANK + 1) as usize;

pub const ECHORAM: u16 = 0xE000;
pub const ECHORAM_END: u16 = OAMRAM - 1;
pub const ECHORAM_SIZE: usize = (ECHORAM_END - ECHORAM + 1) as usize;

pub const OAMRAM: u16 = 0xFE00;
pub const OAMRAM_END: u16 = 0xFE9F;
pub const OAMRAM_SIZE: usize = (OAMRAM_END - OAMRAM + 1) as usize;

pub const IO: u16 = 0xFF00;
pub const IO_END: u16 = 0xFF7F;
pub const IO_SIZE: usize = (IO_END - IO + 1) as usize;

pub const HRAM: u16 = 0xFF80;
pub const HRAM_END: u16 = 0xFFFE;
pub const HRAM_SIZE: usize = (HRAM_END - HRAM + 1) as usize;

pub const P1: u16 = 0xFF00; // Joypad
pub const JOYP: u16 = P1; // Same as P1

pub const SB: u16 = 0xFF01; // Serial transfer data buffer
pub const SC: u16 = 0xFF02; // Serial transfer control

pub const DIV: u16 = 0xFF04; // Divider Register
pub const TIMA: u16 = 0xFF05; // Timer counter
pub const TIM: u16 = 0xFF06; // Timer modulo
pub const TAC: u16 = 0xFF07; // Timer control
pub const IF: u16 = 0xFF0F; // Interrupt flag

pub const NR10: u16 = 0xFF10; // Channel 1 sweep
pub const NR11: u16 = 0xFF11; // Channel 1 length timer & duty cycle
pub const NR12: u16 = 0xFF12; // Channel 1 volume & envelope
pub const NR13: u16 = 0xFF13; // Channel 1 period low [write-only]
pub const NR14: u16 = 0xFF14; // Channel 1 period high & control
pub const NR21: u16 = 0xFF16; // Channel 2 sweep
pub const NR22: u16 = 0xFF17; // Channel 2 length timer & duty cycle
pub const NR23: u16 = 0xFF18; // Channel 2 volume & envelope
pub const NR24: u16 = 0xFF19; // Channel 2 period low [write-only]
pub const NR30: u16 = 0xFF1a; // Channel 3 DAC enable
pub const NR31: u16 = 0xFF1b; // Channel 3 length timer [write-only]
pub const NR32: u16 = 0xFF1c; // Channel 3 output level
pub const NR33: u16 = 0xFF1d; // Channel 3 period low [write-only]
pub const NR34: u16 = 0xFF1e; // Channel 3 period high & control
pub const NR41: u16 = 0xFF20; // Channel 4 length timer [write-only]
pub const NR42: u16 = 0xFF21; // Channel 4 volume & envelope
pub const NR43: u16 = 0xFF22; // Channel 4 frequency & randomness
pub const NR44: u16 = 0xFF23; // Channel 4 control
pub const NR50: u16 = 0xFF24; // Master volume & VIN panning
pub const NR51: u16 = 0xFF25; // Sound panning
pub const NR52: u16 = 0xFF26; // Audio Master Control
pub const WAVE_RAM: u16 = 0xFF30; // Storage for one of the sound channels’ waveform
pub const WAVE_RAM_END: u16 = 0xFF3F;

pub const LCDC: u16 = 0xFF40; // LCD Control
pub const STAT: u16 = 0xFF41; // LCD Status
pub const SCY: u16 = 0xFF42; // Viewport Y position
pub const SCX: u16 = 0xFF43; // Viewport X position
pub const LY: u16 = 0xFF44; // LCD Y coordinate
pub const LYC: u16 = 0xFF45; // LY compare
pub const DMA: u16 = 0xFF46; // OAM DMA source address & start
pub const BGP: u16 = 0xFF47; // BG palette data
pub const OBP0: u16 = 0xFF48; // Object palette 0 data
pub const OBP1: u16 = 0xFF49; // Object palette 1 data
pub const WY: u16 = 0xFF4A; // Window Y position
pub const WX: u16 = 0xFF4B; // Window X position
pub const KEY1: u16 = 0xFF4D; // Speed switch
pub const VBK: u16 = 0xFF4F; // VRAM bank
pub const HDMA1: u16 = 0xFF51; // DMA source high
pub const HDMA2: u16 = 0xFF52; // DMA source low
pub const HDMA3: u16 = 0xFF53; // DMA destination high
pub const HDMA4: u16 = 0xFF54; // DMA destination low
pub const HDMA5: u16 = 0xFF55; // DMA length & start
pub const RP: u16 = 0xFF56; // Infrared port
pub const BCPS: u16 = 0xFF68; // Background color palette specification
pub const BGPI: u16 = 0xFF68; // Background palette index
pub const BCPD: u16 = 0xFF69; // Background color palette data
pub const BGPD: u16 = 0xFF69; // Background palette data
pub const OCPS: u16 = 0xFF6A; // OBJ color palette specification
pub const OBPI: u16 = 0xFF6A; // OBJ palette index
pub const OCPD: u16 = 0xFF6B; // OBJ color palette data
pub const OBPD: u16 = 0xFF6B; // OBJ palette data
pub const OPRI: u16 = 0xFF6C; // 	Object priority mode
pub const SVBK: u16 = 0xFF70; // WRAM bank
pub const PCM12: u16 = 0xFF76; // Audio digital outputs 1 & 2
pub const PCM34: u16 = 0xFF77; // Audio digital outputs 3 & 4
pub const IE: u16 = 0xFFFF; // Interrupt enable
