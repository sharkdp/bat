#[cfg(not(target_os = "wasi"))]
pub(crate) use clircle::{Clircle, Identifier, Stdio};

#[cfg(target_os = "wasi")]
pub(crate) use wasi::{Clircle, Identifier, Stdio};

#[cfg(target_os = "wasi")]
mod wasi {
    use rustix::fs::{fstat, seek, FileType, SeekFrom, Stat};
    use std::fs::File;
    use std::io;

    pub(crate) enum Stdio {
        Stdin,
        Stdout,
    }

    pub(crate) trait Clircle {
        fn stdout() -> Option<Self>
        where
            Self: Sized;
        fn surely_conflicts_with(&self, other: &Self) -> bool;
        fn into_inner(self) -> Option<File>;
    }

    pub(crate) struct Identifier {
        device: u64,
        inode: u64,
        regular: bool,
        file: Option<File>,
        unread: bool,
    }

    impl Identifier {
        fn from_stat(stat: Stat, position: Option<u64>, file: Option<File>) -> Self {
            Self {
                device: stat.st_dev,
                inode: stat.st_ino,
                regular: FileType::from_raw_mode(stat.st_mode) == FileType::RegularFile,
                unread: position
                    .zip(u64::try_from(stat.st_size).ok())
                    .map(|(position, size)| position < size)
                    .unwrap_or(true),
                file,
            }
        }
    }

    impl TryFrom<Stdio> for Identifier {
        type Error = io::Error;
        fn try_from(stream: Stdio) -> io::Result<Self> {
            let fd = match stream {
                Stdio::Stdin => rustix::stdio::stdin(),
                Stdio::Stdout => rustix::stdio::stdout(),
            };
            Ok(Self::from_stat(
                fstat(fd)?,
                seek(fd, SeekFrom::Current(0)).ok(),
                None,
            ))
        }
    }

    impl TryFrom<File> for Identifier {
        type Error = io::Error;
        fn try_from(file: File) -> io::Result<Self> {
            Ok(Self::from_stat(
                fstat(&file)?,
                seek(&file, SeekFrom::Current(0)).ok(),
                Some(file),
            ))
        }
    }

    impl Clircle for Identifier {
        fn stdout() -> Option<Self> {
            Self::try_from(Stdio::Stdout).ok()
        }
        fn surely_conflicts_with(&self, other: &Self) -> bool {
            self.regular
                && other.regular
                && self.device == other.device
                && self.inode == other.inode
                && other.unread
        }
        fn into_inner(self) -> Option<File> {
            self.file
        }
    }
}
