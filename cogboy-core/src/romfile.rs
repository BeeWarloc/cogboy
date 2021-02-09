use std::sync::Mutex;
use std::collections::HashMap;
use std::ops::Index;
use serde::{self, Deserializer, Deserialize, Serializer, Serialize};
use serde::de::Visitor;
use std::sync::{Arc, Weak};
use std::fmt;
use std::fs::File;
use std::io::{Read, Error as IOError};

lazy_static! {
    static ref ROM_CACHE: Mutex<HashMap<String, Weak<Vec<u8>>>> = HashMap::new().into() ;
}

#[derive(Debug, Clone)]
pub struct RomFile {
    pub bytes: Arc<Vec<u8>>,
    path: String,
}

impl RomFile {
    pub fn load(path: &str) -> Result<RomFile, IOError> {
        let mut cache = ROM_CACHE.lock().expect("Rom cache couldn't be unlocked, this should not happen");
        if let Some(Some(bytes)) = cache.get(path).map(|bytes_weak| bytes_weak.upgrade()) {
            return Ok(RomFile { bytes: bytes, path: path.to_string() });
        }

        let mut f = File::open(path)?;
        let mut bytes: Vec<u8> = Vec::new();
        f.read_to_end(&mut bytes)?;
        println!("Loaded ROM with MBC type {:02x} ROM SIZE (ID) {:02x} RAM SIZE (ID) {:02x}",
            bytes[0x147],
            bytes[0x148],
            bytes[0x149]);

        let bytes = Arc::new(bytes);
        cache.insert(path.to_string(),  Arc::downgrade(&bytes));
        Ok(RomFile { bytes: bytes.into(), path: path.to_string() })
    }
}

impl Index<usize> for RomFile {
    type Output = u8;

    #[inline]
    fn index(&self, index: usize) -> &u8 {
        &self.bytes[index]
    }
}

impl Serialize for RomFile {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer,
    {
        self.path.serialize(serializer)
    }
}

struct RomFileVisitor;


impl<'de> Visitor<'de> for RomFileVisitor {
    type Value = RomFile;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a path to rom file")
    }

    fn visit_str<E>(self, path: &str) -> Result<Self::Value, E>
        where
        E: serde::de::Error
    {
        RomFile::load(path).map_err(|_| E::custom(format!("ROM could not be loaded from path: {}", path)))
    }
}

impl<'de> Deserialize<'de> for RomFile {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer<'de>
    {
        deserializer.deserialize_str(RomFileVisitor)
    }
}