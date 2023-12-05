#![no_main]

use arbitrary::Arbitrary;
use bat::{assets::HighlightingAssets, config::Config, controller::Controller, Input};
use libfuzzer_sys::fuzz_target;
use std::hint::black_box;

#[derive(Arbitrary, Debug)]
struct Ctx<'a> {
    file_name: &'a str,
    file_contents: &'a [u8],
    config: Config<'a>,
}

fuzz_target!(|ctx: Ctx| {
    let mut buffer = String::new();
    let assets = HighlightingAssets::from_binary();
    let controller = Controller::new(&ctx.config, &assets);
    let input = Input::from_bytes(ctx.file_contents).name(ctx.file_name);
    let _ = black_box(controller.run(vec![input.into()], Some(&mut buffer)));
});
