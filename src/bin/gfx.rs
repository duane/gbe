use bevy::{
    prelude::*,
    render::render_resource::PrimitiveTopology,
    window::{EnabledButtons, WindowResized, WindowResolution},
};
use gbc::gfx::screen;

const MIN_SCALE_FACTOR: usize = 5;

const MAX_SCALE_FACTOR: usize = 5;
const MIN_WIDTH: usize = screen::WIDTH * MIN_SCALE_FACTOR;
const MAX_WIDTH: usize = screen::WIDTH * MAX_SCALE_FACTOR;
const MIN_HEIGHT: usize = screen::HEIGHT * MIN_SCALE_FACTOR;
const MAX_HEIGHT: usize = screen::HEIGHT * MAX_SCALE_FACTOR;

fn main() {
    let default_resolution = WindowResolution::new(MIN_WIDTH as f32, MIN_HEIGHT as f32);
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
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
        }))
        .add_plugins(gbc::gfx::screen::ColoredMesh2dPlugin)
        .add_systems(Startup, screen::screen)
        .add_systems(Update, on_window_resize)
        .run();
}

fn on_window_resize(mut resize_reader: EventReader<WindowResized>) {
    if let Some(ref resized) = resize_reader.read().next() {
        let width = resized.width;
        let height = resized.height;
        println!("width: {}", width);
        println!("height: {}", height);
    }
}
