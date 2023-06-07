const Size = struct {
    columns: u16,
    rows: u16,
};

pub fn getTerminalSize() !Size {
    const builtin = @import("builtin");
    switch (builtin.os.tag) {
        .linux => return linuxGetTerminalSize(),
        .windows => return windowsGetTerminalSize(),
        else => @compileError("unsupported OS " ++ @typeName(builtin.os.tag)),
    }
}

fn linuxGetTerminalSize() !Size {
    var wsz: std.os.linux.winsize = undefined;
    const rc = std.os.linux.ioctl(std.io.getStdOut().handle, std.os.linux.T.IOCGWINSZ, @ptrToInt(&wsz));
    switch (std.os.linux.getErrno(rc)) {
        .SUCCESS => {},
        else => return error.GetTerminalSizeFailed,
    }
    return .{ .columns = wsz.ws_col, .rows = wsz.ws_row };
}

fn windowsGetTerminalSize() !Size {
    var info: std.os.windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
    if (std.os.windows.kernel32.GetConsoleScreenBufferInfo(std.io.getStdOut().handle, &info) != std.os.windows.TRUE) {
        return error.GetTerminalSizeFailed;
    }
    return .{
        .columns = @intCast(u16, info.srWindow.Right - info.srWindow.Left + 1),
        .rows = @intCast(u16, info.srWindow.Bottom - info.srWindow.Top + 1),
    };
}

fn macosGetTerminalSize() !Size {
    var wsz: std.os.darwin.winsize = undefined;
    const rc = std.os.darwin.ioctl(std.io.getStdOut().handle, std.os.darwin.T.IOCGWINSZ, &wsz);
    switch (std.os.darwin.getErrno(rc)) {
        .SUCCESS => {},
        else => return error.GetTerminalSizeFailed,
    }
    return .{ .columns = wsz.ws_col, .rows = wsz.ws_row };
}

const std = @import("std");
