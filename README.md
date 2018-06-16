# CogBoy

CogBoy is a rather incomplete Gameboy emulator I started writing as a project to learn rust.

It does seem to start and run the majority of games I throw at it, with a varying degree of correctness and stability.

### Dependencies

Makes use of [PortAudio](http://www.portaudio.com/) through the
[rust-portaudio](https://github.com/RustAudio/rust-portaudio) bindings. Thank you, PortAudio!

A prebuilt binary of PortAudio for 64 bit Windows is included for convenience, as I couldn't find any precompiled of a recent version. PortAudio is licensed under a MIT like license, more details can be found [here](http://www.portaudio.com/license.html).

For other platforms just make sure that the PortAudio is installed, see their docs for [details](http://www.portaudio.com/).

### Compiling and running

`cargo run --release path/to/some/rom.gb`

### Limitations

- Only DMG (the original Gameboy) is emulated
- Sound channel 3 (wave) and 4 (noise)
- External RAM bank select
- No save games

You should probably use one of the many better and more complete Just putting it out here for reference and backup. ;)

### License

Licensed under the MIT license, see the `LICENSE` file for details.

### Useful links

- [GB pan docs](http://bgb.bircd.org/pandocs.htm)
- [Opcode table](http://pastraiser.com/cpu/gameboy/gameboy_opcodes.html)

