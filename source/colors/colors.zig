pub const colors: type = struct {
    pub const reset: *const [4:0]u8 = "\x1b[0m";
    pub const white: *const [5:0]u8 = "\x1b[37m";
    pub const green: *const [5:0]u8 = "\x1b[32m";
    pub const red: *const [5:0]u8 = "\x1b[31m";
    pub const bold: *const [4:0]u8 = "\x1b[1m";
};
