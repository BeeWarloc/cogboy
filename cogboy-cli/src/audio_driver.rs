use super::{ControlMessage, SoundMessage};
use portaudio as pa;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};

const CHANNELS: i32 = 2;
const SAMPLE_RATE: f64 = 44_100.0;
const FRAMES_PER_BUFFER: u32 = 128;

const GAMEBOY_SAMPLE_RATE: usize = 1 << 20;

struct StreamCallbackState {
    queue: VecDeque<(f32, f32)>,
    snd_rx: Receiver<SoundMessage>,
    message_tx: Sender<ControlMessage>,
    subsample_counter: f64,
    subsample_step: f64,
    paused: bool,
}

pub fn init(
    snd_rx: Receiver<SoundMessage>,
    message_tx: Sender<ControlMessage>,
) -> Result<pa::Stream<pa::NonBlocking, pa::Output<f32>>, pa::Error> {
    let pa = pa::PortAudio::new()?;

    let mut settings =
        pa.default_output_stream_settings(CHANNELS, SAMPLE_RATE, FRAMES_PER_BUFFER)?;
    // we won't output out of range samples so don't bother clipping them.
    settings.flags = pa::stream_flags::CLIP_OFF;

    message_tx
        .send(ControlMessage::Tick(
            (FRAMES_PER_BUFFER as f64 * (1 << 22) as f64 / SAMPLE_RATE) as i32,
        ))
        .unwrap();

    let state = StreamCallbackState {
        queue: VecDeque::with_capacity(1024),
        snd_rx: snd_rx,
        message_tx: message_tx,
        paused: false,
        subsample_counter: 0.0,
        subsample_step: SAMPLE_RATE as f64 / GAMEBOY_SAMPLE_RATE as f64,
    };

    let state = RefCell::new(state);

    // This routine will be called by the PortAudio engine when audio is needed. It may called at
    // interrupt level on some machines so don't do anything that could mess up the system like
    // dynamic resource allocation or IO.
    let callback = move |pa::OutputStreamCallbackArgs { buffer, frames, .. }| {
        let mut state = state.borrow_mut();
        loop {
            match state.snd_rx.try_recv() {
                Ok(SoundMessage::Buffer(incoming)) => {
                    for s in incoming {
                        while state.subsample_counter >= 1.0 {
                            state.queue.push_back((
                                s.0 as f32 * (1.0 / 127.0),
                                s.1 as f32 * (1.0 / 127.0),
                            ));
                            state.subsample_counter -= 1.0;
                        }
                        state.subsample_counter += state.subsample_step;
                    }
                }
                Ok(SoundMessage::Play) => state.paused = false,
                Ok(SoundMessage::Pause) => state.paused = true,
                Err(mpsc::TryRecvError::Empty) => break,
                Err(mpsc::TryRecvError::Disconnected) => return pa::Abort,
            }
        }
        while state.queue.len() > 44100 {
            state.queue.pop_front();
        }
        if state.paused {
            for i in 0..frames {
                buffer[i * 2] = 0.0;
                buffer[i * 2 + 1] = 0.0;
            }
        } else {
            for i in 0..frames {
                let s = match state.queue.pop_front() {
                    Some(x) => x,
                    None => (0.0f32, 0.0f32),
                };
                buffer[i * 2] = s.0;
                buffer[i * 2 + 1] = s.1;
            }

            let queue_target = FRAMES_PER_BUFFER as usize * 6;
            let queue_len = state.queue.len();
            if queue_len < queue_target {
                state
                    .message_tx
                    .send(ControlMessage::Tick(
                        ((queue_target - queue_len) as f64 / state.subsample_step) as i32,
                    ))
                    .unwrap();
            }
        }

        pa::Continue
    };

    let mut stream = pa.open_non_blocking_stream(settings, callback)?;

    stream.start()?;

    Ok(stream)
}
