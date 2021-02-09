use std::io::{self, Write, Read};
use serde::Deserialize;
use serde::Serialize;
use std::marker::PhantomData;
use bincode::{deserialize_from, serialize_into};

#[derive(Clone)]
pub struct Zlob<T> {
    bytes: Box<[u8]>,
    phantom: PhantomData<T>,
}

use flate2;

fn new_encoder<W: Write>(sink: W) -> impl Write {
    flate2::write::DeflateEncoder::new(sink, flate2::Compression::default())
}

fn new_decoder<R: Read>(src: R) -> impl Read {
    flate2::read::DeflateDecoder::new(src)
}

impl<T> Zlob<T> where for<'de> T: Deserialize<'de> {
    pub fn size(&self) -> usize {
        self.bytes.len()
    }

    pub fn read_decompressed_bytes(&self, writer: &mut impl Write) {
        let reader = self.bytes.as_ref();
        let mut reader = new_decoder(reader);
        io::copy(&mut reader, writer).unwrap();
    }

    pub fn count_diff_bytes(&self, other: &Self) -> usize {
        let self_reader = new_decoder(self.bytes.as_ref());
        let other_reader = new_decoder(other.bytes.as_ref());
        let diffstream: Vec<u8> = self_reader.bytes().filter_map(|x| x.ok())
            .zip(other_reader.bytes().filter_map(|x| x.ok())).map(|(a,b)| a.wrapping_sub(b)).collect();
        let mut diffvec: Vec<u8> = Vec::new();
        {
            let mut writer = new_encoder(&mut diffvec);
            io::copy(&mut &diffstream[..], &mut writer).unwrap();
        }
        return diffvec.len();
    }

    pub fn deserialize(&self) -> T {
        let reader = self.bytes.as_ref();
        let mut reader = new_decoder(reader);
        deserialize_from(&mut reader).unwrap()
    }
}

impl<'a, T> From<&'a T> for Zlob<T>
    where T: Serialize {
    fn from(value: &'a T) -> Self {
        let mut writer = Vec::new();
        {
            let mut writer = new_encoder(&mut writer);
            serialize_into(&mut writer, value).expect("Failed serializing");
        }
        Zlob { bytes: writer.into_boxed_slice(), phantom: PhantomData }
    }
}
