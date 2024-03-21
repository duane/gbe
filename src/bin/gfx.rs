#![allow(clippy::nonminimal_bool)]
#![allow(clippy::needless_update)]
#![allow(clippy::unit_arg)]
#![allow(clippy::unused_io_amount)]

use std::{
    env::args,
    fs::File,
    io::{BufReader, Read},
};

use bevy::{
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    prelude::*,
    window::{EnabledButtons, WindowResolution},
};
use bevy_pixels::{PixelsOptions, PixelsPlugin, PixelsWrapper};
use color_eyre::Result;
use gbc::{
    machine::Machine,
    ppu::{HEIGHT, UPDATES_PER_SECOND, WIDTH},
    rom::ROM,
};

const MAX_SCALE_FACTOR: usize = 5;
const MIN_WIDTH: usize = WIDTH * MAX_SCALE_FACTOR;
const MAX_WIDTH: usize = WIDTH * MAX_SCALE_FACTOR;
const MIN_HEIGHT: usize = HEIGHT * MAX_SCALE_FACTOR;
const MAX_HEIGHT: usize = HEIGHT * MAX_SCALE_FACTOR;
#[cfg(target_arch = "wasm32")]
const BUF: &[u8] = include_bytes!("../../assets/roms/pokemon-red.gb");
#[cfg(not(target_arch = "wasm32"))]
const BUF: &[u8] = &[0; 0x8000];

fn main() -> Result<()> {
    color_eyre::install()?;
    let args = args().collect::<Vec<String>>();
    if args.len() > 2 {
        println!("Usage: gbc [rom file]");
        return Ok(());
    }
    #[cfg(target_arch = "wasm32")]
    let rom = ROM::from_buf(BUF.to_vec());
    #[cfg(not(target_arch = "wasm32"))]
    let rom = if args.len() > 1 {
        let file = File::open(&args[1]);
        if file.is_err() {
            println!("Error opening file: {}", args[1]);
            return Err(file.err().unwrap().into());
        }
        let file = file.unwrap();
        let mut reader = BufReader::new(file);
        let mut rom_file = vec![0x0; 0x8000];
        reader.read(&mut rom_file).unwrap();
        println!("Loaded ROM: {}", args[1]);
        ROM::from_buf(rom_file)
    } else {
        ROM::from_buf(BUF.to_vec())
    };
    let machine = gbc::machine::Machine::new(rom);

    let default_resolution: WindowResolution =
        WindowResolution::new(MIN_WIDTH as f32, MIN_HEIGHT as f32);

    Ok(App::new()
        .insert_resource(BevyMachine {
            machine,
            err: Ok(()),
        })
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    resolution: default_resolution,
                    resize_constraints: WindowResizeConstraints {
                        min_width: MIN_WIDTH as f32,
                        min_height: MIN_HEIGHT as f32,
                        max_width: MAX_WIDTH as f32,
                        max_height: MAX_HEIGHT as f32,
                    },
                    enabled_buttons: EnabledButtons {
                        close: true,
                        minimize: true,
                        maximize: false,
                    },
                    ..default()
                }),
                ..default()
            }),
            PixelsPlugin {
                primary_window: Some(PixelsOptions {
                    width: WIDTH as u32,
                    height: HEIGHT as u32,
                    auto_resize_buffer: false,
                    auto_resize_surface: true,
                    scale_factor: 0.5,
                    ..default()
                }),
            },
            FrameTimeDiagnosticsPlugin,
            LogDiagnosticsPlugin::default(),
        ))
        .add_systems(FixedUpdate, step_frame)
        .add_systems(Update, draw)
        .insert_resource(Time::<Fixed>::from_seconds(UPDATES_PER_SECOND))
        .run())
}

#[derive(Resource)]
struct BevyMachine {
    machine: Machine,
    err: Result<()>,
}

fn draw(mut wrapper_query: Query<&mut PixelsWrapper>, machines: ResMut<BevyMachine>) {
    // Query the `PixelsWrapper` component that owns an instance of `Pixels` for the given window.
    let Ok(mut wrapper) = wrapper_query.get_single_mut() else {
        println!("failed to get wrapper :(");
        return;
    };

    // Get a mutable slice for the pixel buffer.
    let frame: &mut [u8] = wrapper.pixels.frame_mut();
    let machine = &machines.machine;
    frame.copy_from_slice(&machine.cpu.bus.ppu.frame);
}

fn step_frame(mut machines: ResMut<BevyMachine>) {
    if !machines.err.is_err() {
        if let err @ Err(_) = machines.machine.step_frame() {
            println!("FATAL ERROR: {:?}", err);
            machines.err = err;
        }
    }
}
