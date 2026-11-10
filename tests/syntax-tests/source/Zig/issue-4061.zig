pub const Error = error{
    IoerrDir_fsync,
    ioerrshmmap,
};

const existing = error.IoerrDir_fsync;