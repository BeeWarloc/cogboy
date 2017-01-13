# CogBoy

A basic Gameboy emulator I made to learn a bit of rust.

### State

It's nowhere near complete, although it does run a couple of roms OK. You should really not use this, as there are tons of better alternatives out there.

Just putting it out here for reference and backup. ;)

### Dependencies

Makes use of [PortAudio](http://www.portaudio.com/) through the
[rust-portaudio](https://github.com/RustAudio/rust-portaudio) bindings. Thank you, PortAudio!

A prebuilt binary of PortAudio for 64 bit Windows is included for convenience, as I couldn't find any precompiled of a recent version. PortAudio is licensed under a MIT like license, more details can be found [here](http://www.portaudio.com/license.html).

For other platforms just make sure that the PortAudio is installed, see their docs for [details](http://www.portaudio.com/).

### License

Licensed under the MIT license, see the `LICENSE` file for details.

### Useful links

- [GB pan docs](http://bgb.bircd.org/pandocs.htm)
- [Opcode table](http://pastraiser.com/cpu/gameboy/gameboy_opcodes.html)

