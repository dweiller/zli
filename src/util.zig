const Size = struct {
    columns: u16,
    rows: u16,
};

pub fn getTerminalSize() ?Size {
    const builtin = @import("builtin");
    switch (builtin.os.tag) {
        .linux => return linuxGetTerminalSize(),
        .windows => return windowsGetTerminalSize(),
        else => @compileError("unsupported OS " ++ @tagName(builtin.os.tag)),
    }
}

fn linuxGetTerminalSize() ?Size {
    var size: std.posix.winsize = undefined;
    const rc = std.os.linux.ioctl(
        std.io.getStdOut().handle,
        std.os.linux.T.IOCGWINSZ,
        @intFromPtr(&size),
    );
    switch (std.os.linux.E.init(rc)) {
        .SUCCESS => {},
        else => return null,
    }
    return .{ .columns = size.col, .rows = size.row };
}

fn windowsGetTerminalSize() ?Size {
    var info: std.os.windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
    if (std.os.windows.kernel32.GetConsoleScreenBufferInfo(std.io.getStdOut().handle, &info) != std.os.windows.TRUE) {
        return null;
    }
    return .{
        .columns = @intCast(info.srWindow.Right - info.srWindow.Left + 1),
        .rows = @intCast(info.srWindow.Bottom - info.srWindow.Top + 1),
    };
}

const std = @import("std");
