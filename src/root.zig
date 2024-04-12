const zli = @This();

pub const CliOptions = struct {
    include_defaults: bool = true,
    parameters: []const Arg = &.{},
    help_message: []const u8 = &.{},
};

pub fn CliCommand(
    comptime name: []const u8,
    comptime version: std.SemanticVersion,
    comptime options: CliOptions,
) type {
    checkNameClash(options.parameters, options.include_defaults);

    return struct {
        pub const args = if (options.include_defaults)
            argsWithDefaults(options.parameters)
        else
            options.parameters;
        pub const ParsedResult = ParseResult(args);
        pub const Params = ParsedResult.Params;

        const longest_arg_name = length: {
            var length = 0;
            for (args) |arg| {
                const arg_name = arg.fieldName();
                if (arg_name.len > length) {
                    length = arg_name.len;
                }
            }
            break :length length;
        };

        pub fn parse(allocator: std.mem.Allocator) Allocator.Error!ParsedResult {
            return zli.parse(allocator, args);
        }

        pub fn parseOrExit(allocator: std.mem.Allocator, status: u8) ParsedResult {
            return zli.parse(allocator, args) catch |err| {
                std.log.err("{s}", .{@errorName(err)});
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpStackTrace(trace.*);
                }
                std.process.exit(status);
            };
        }

        pub fn parseWithArgs(
            allocator: Allocator,
            args_iter: *ArgIterator,
        ) Allocator.Error!ParsedResult {
            return zli.parseWithArgs(allocator, args, args_iter);
        }

        pub fn printHelpAndExit() noreturn {
            const stdout = std.io.getStdOut();
            const columns: ?usize = if (stdout.isTty())
                (getTerminalSize() catch std.process.exit(1)).columns
            else
                null;

            printHelp(
                stdout.writer(),
                columns,
                .{
                    .name = name,
                    .version = version,
                    .help_message = options.help_message,
                    .longest_arg_len = longest_arg_name,
                    .args = options.parameters,
                },
            ) catch std.process.exit(1);
            std.process.exit(0);
        }

        pub fn printVersionAndExit() noreturn {
            const writer = std.io.getStdOut().writer();
            writeVersion(writer, name, version, "") catch std.process.exit(1);
            std.process.exit(0);
        }
    };
}

fn printHelp(
    writer: anytype,
    columns: ?usize,
    comptime options: struct {
        name: []const u8,
        version: std.SemanticVersion,
        help_message: []const u8,
        longest_arg_len: comptime_int,
        args: []const Arg,
    },
) !void {
    try writer.print("{s} {}\n", .{ options.name, options.version });
    if (options.help_message.len > 0)
        try writer.writerAll(options.help_message);
    try writer.writeAll("\nOptions:\n\n");
    try writeOptions(
        writer,
        options.longest_arg_len,
        columns,
        40,
        60,
        options.args,
    );
}

const default_args = [_]Arg{
    .{
        .name = .{ .long = .{ .full = "help" } },
        .short_help = "Print this help message",
        .type = bool,
    },
    .{
        .name = .{ .long = .{ .full = "version" } },
        .short_help = "Print version information",
        .type = bool,
    },
};

pub fn argsWithDefaults(comptime args: []const Arg) []const Arg {
    return args ++ default_args;
}

pub fn writeOptions(
    writer: anytype,
    comptime longest: comptime_int,
    columns: ?usize,
    min_width: usize,
    max_col_width: usize,
    comptime spec: []const Arg,
) !void {
    const long_fmt = std.fmt.comptimePrint("{{s: <{d}}}", .{longest});

    const separator = " " ** 8;
    const indent_size = 2;
    const help_padding = indent_size + separator.len + longest + "  -h, --".len;

    const max_width = if (columns) |width| max: {
        if (width < min_width + help_padding) break :max null;
        break :max @min(width, help_padding + max_col_width);
    } else null;

    const option_fmt_short_long = "  -{c}, --" ++ long_fmt ++ separator;
    const option_fmt_long = "      --" ++ long_fmt ++ separator;
    const option_fmt_short = "  -{c}    " ++ [_]u8{' '} ** longest ++ separator;

    inline for (spec) |arg| {
        if (max_width) |width| switch (arg.name) {
            .long => |n| if (n.short) |short| {
                try writer.print(option_fmt_short_long, .{ short, n.full });
                try writeAlignedTo(writer, help_padding, width, arg.short_help);
            } else {
                try writer.print(option_fmt_long, .{n.full});
                try writeAlignedTo(writer, help_padding, width, arg.short_help);
            },
            .short => |c| {
                try writer.print(option_fmt_short, .{c});
                try writeAlignedTo(writer, help_padding, width, arg.short_help);
            },
        } else switch (arg.name) {
            .long => |n| if (n.short) |short|
                try writer.print(option_fmt_short_long ++ "{s}\n", .{ short, n.full, arg.short_help })
            else
                try writer.print(option_fmt_long ++ "{s}\n", .{ n.full, arg.short_help }),
            .short => |c| try writer.print(option_fmt_short ++ "{s}\n", .{ c, arg.short_help }),
        }
    }
}

fn writeAlignedTo(writer: anytype, padding: usize, end: usize, bytes: []const u8) !void {
    std.debug.assert(end > padding);

    const width = end - padding;

    var index: usize = 0;
    var current: usize = 0;
    // first line doesn't need padding
    while (std.mem.indexOfScalarPos(u8, bytes, current, ' ')) |i| : (current = i + 1) {
        if (i - index > width) {
            try writer.print("{s}\n", .{bytes[index .. current - 1]});
            index = current;
            break;
        }
    } else {
        try writer.print("{s}\n", .{bytes});
        return;
    }

    while (std.mem.indexOfScalarPos(u8, bytes, current, ' ')) |i| : (current = i + 1) {
        if (i - index > width) {
            try writer.writeByteNTimes(' ', padding);
            try writer.print("{s}\n", .{bytes[index .. current - 1]});
            index = current;
        }
    } else {
        try writer.writeByteNTimes(' ', padding);
        try writer.print("{s}\n", .{bytes[index..]});
    }
}

pub fn writeVersion(
    writer: anytype,
    cannonical_name: []const u8,
    version: std.SemanticVersion,
    other: []const u8,
) !void {
    try writer.print("{s} {}\n", .{ cannonical_name, version });
    if (other.len > 0) try writer.print("{s}\n", .{other});
}

pub fn ParseResult(comptime spec: []const Arg) type {
    return union(enum) {
        ok: Params,
        err: Err,

        pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            switch (self) {
                .ok => |ok| ok.deinit(allocator),
                .err => |err| allocator.free(err.string),
            }
        }

        pub const Params = struct {
            options: Options(spec),
            positional: []const [:0]const u8,

            pub fn deinit(self: Params, allocator: std.mem.Allocator) void {
                inline for (std.meta.fields(@TypeOf(self.options))) |field| {
                    const Child = @typeInfo(field.type).Optional.child;
                    if (@typeInfo(Child) == .Pointer) {
                        if (@field(self.options, field.name)) |slice| {
                            allocator.free(slice);
                        }
                    }
                }
                for (self.positional) |arg| allocator.free(arg);
                allocator.free(self.positional);
            }
        };

        pub const Err = struct {
            arg_name: []const u8,
            string: []const u8,
            err: Error,

            pub fn format(
                value: Err,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = fmt;
                _ = options;
                switch (value.err) {
                    error.Missing => try writer.print(
                        "missing value for parameter {s}",
                        .{value.arg_name},
                    ),
                    error.BadValue => try writer.print(
                        "invalid value for parameter {s}",
                        .{value.arg_name},
                    ),
                    error.Unrecognized => try writer.print(
                        "unrecognized parameter '{s}'",
                        .{value.string},
                    ),
                    error.InvalidCharacter => try writer.print(
                        "value '{s}' for parameter {s} contains an invalid character",
                        .{ value.string, value.arg_name },
                    ),
                    error.Overflow => try writer.print(
                        "value '{s}' for parameter {s} is out of bounds (try something closer to 0)",
                        .{ value.string, value.arg_name },
                    ),
                }
            }
        };
    };
}

pub const Error = error{ Missing, BadValue, Unrecognized } ||
    std.fmt.ParseIntError || std.fmt.ParseFloatError;

pub fn parseWithArgs(
    allocator: std.mem.Allocator,
    comptime args: []const Arg,
    args_iter: *std.process.ArgIterator,
) !ParseResult(args) {
    var options = Options(args){};
    var positional = std.ArrayList([:0]u8).init(allocator);

    while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--")) break;
        inline for (args) |s| {
            const match = s.name.matches(arg);
            const embeded_value = if (s.name == .long and match == .long_with_eql)
                arg[2 + s.name.long.full.len + 1 ..]
            else
                null;

            if (match != .no) {
                const field_name = std.fmt.comptimePrint("{s}", .{s.name});
                @field(options, field_name) = switch (@typeInfo(s.type)) {
                    .Int, .Float, .Pointer, .Enum => f: {
                        const value = embeded_value orelse args_iter.next() orelse return .{
                            .err = .{
                                .arg_name = s.paramName(),
                                .string = "",
                                .err = Error.Missing,
                            },
                        };
                        switch (@typeInfo(s.type)) {
                            .Int => break :f std.fmt.parseInt(s.type, value, 0) catch |err| return .{
                                .err = .{
                                    .arg_name = s.paramName(),
                                    .string = try allocator.dupe(u8, value),
                                    .err = err,
                                },
                            },
                            .Float => break :f std.fmt.parseFloat(s.type, value) catch |err| return .{
                                .err = .{
                                    .arg_name = s.paramName(),
                                    .string = try allocator.dupe(u8, value),
                                    .err = err,
                                },
                            },
                            .Pointer => |p| switch (p.size) {
                                .One, .Many, .C => failCompilationBadType(s.type),
                                .Slice => {
                                    if (p.child != u8) failCompilationBadType(s.type);
                                    break :f try allocator.dupeZ(u8, value);
                                },
                            },
                            .Enum => break :f std.meta.stringToEnum(s.type, value) orelse return .{
                                .err = .{
                                    .arg_name = s.paramName(),
                                    .string = try allocator.dupe(u8, value),
                                    .err = Error.BadValue,
                                },
                            },
                            else => unreachable,
                        }
                    },
                    .Bool => true,
                    else => failCompilationBadType(s.type),
                };
                break;
            }
        } else {
            if (arg[0] == '-' and arg.len > 1) return .{
                .err = .{
                    .arg_name = "",
                    .string = arg,
                    .err = Error.Unrecognized,
                },
            } else {
                try positional.append(try allocator.dupeZ(u8, arg));
            }
        }
    }
    while (args_iter.next()) |arg| {
        try positional.append(try allocator.dupeZ(u8, arg));
    }
    return .{ .ok = .{
        .options = options,
        .positional = try positional.toOwnedSlice(),
    } };
}

pub fn parse(
    allocator: std.mem.Allocator,
    comptime args: []const Arg,
) Allocator.Error!ParseResult(args) {
    var args_iter = try std.process.argsWithAllocator(allocator);
    defer args_iter.deinit();
    _ = args_iter.skip();
    return parseWithArgs(allocator, args, &args_iter);
}

pub const ArgName = union(enum) {
    long: struct { full: [:0]const u8, short: ?u8 = null },
    short: u8,

    pub fn format(
        value: ArgName,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (value) {
            .long => |n| try writer.writeAll(n.full),
            .short => |n| try writer.writeByte(n),
        }
    }

    pub const MatchResult = enum {
        yes,
        no,
        long_with_eql,
    };

    pub fn matches(name: ArgName, string: []const u8) MatchResult {
        if (string.len > 3 and std.mem.startsWith(u8, string, "--"))
            return name.matchesLong(string[2..]);
        if (string.len == 2 and string[0] == '-')
            return if (name.matchesShort(string[1])) .yes else .no;
        return .no;
    }

    fn matchesLong(name: ArgName, string: []const u8) MatchResult {
        switch (name) {
            .long => |n| {
                if (string.len > 1) {
                    if (std.mem.startsWith(u8, string, n.full)) {
                        if (string.len == n.full.len)
                            return .yes
                        else if (string.len > n.full.len + 2 and string[n.full.len] == '=')
                            return .long_with_eql;
                    }
                } else if (n.short == string[0])
                    return .yes;
                return .no;
            },
            .short => return .no,
        }
    }

    fn matchesShort(name: ArgName, char: u8) bool {
        return switch (name) {
            .long => |n| n.short == char,
            .short => |n| n == char,
        };
    }

    fn hasClash(name1: ArgName, name2: ArgName) bool {
        if (name1 == .short and name2 == .short) {
            return name1.short == name2.short;
        } else if (name1 == .long and name2 == .long) {
            return std.mem.eql(u8, name1.long.full, name2.long.full) or
                name1.long.short != null and name1.long.short == name2.long.short;
        } else if (name1 == .long) {
            return name1.long.short == name2.short;
        } else if (name2 == .long) {
            return name1.short == name2.long.short;
        }
        unreachable;
    }
};

pub const Arg = struct {
    name: ArgName,
    short_help: []const u8,
    type: type = []const u8,

    pub fn fieldName(comptime arg: Arg) [:0]const u8 {
        return switch (arg.name) {
            .long => |n| n.full,
            .short => |c| std.fmt.comptimePrint("{c}", .{c}),
        };
    }

    pub fn paramName(comptime arg: Arg) []const u8 {
        return switch (arg.name) {
            .long => |n| if (n.short) |c| "-" ++ .{c} ++ "/--" ++ n.full else "--" ++ n.full,
            .short => |c| "-" ++ .{c},
        };
    }
};

fn checkNameClash(comptime args: []const Arg, comptime check_defaults: bool) void {
    for (args, 0..) |arg, i| {
        if (arg.name == .long and arg.name.long.full.len == 1) {
            @compileError("A long argument's full name must have length greater than 1");
        }
        switch (@typeInfo(arg.type)) {
            .Int, .Float, .Bool, .Enum => {},
            .Pointer => |p| {
                switch (p.size) {
                    .One, .Many, .C => failCompilationBadType(arg.type),
                    .Slice => if (p.child != u8) failCompilationBadType(arg.type),
                }
            },
            else => failCompilationBadType(arg.type),
        }
        for (args[i + 1 ..], 0..) |t, j| {
            if (arg.name.hasClash(t.name)) {
                @compileError(std.fmt.comptimePrint("arguments {d} and {d} have a name clash", .{ i, j }));
            }
        }
        if (check_defaults)
            for (default_args) |t| {
                if (arg.name.hasClash(t.name)) {
                    @compileError(std.fmt.comptimePrint("argument {s} has a name clash with default parameter {s}", .{ arg.fieldName(), t.fieldName() }));
                }
            };
    }
}

pub fn Options(comptime args: []const Arg) type {
    var fields: [args.len]std.builtin.Type.StructField = undefined;
    for (&fields, args) |*f, s| {
        f.* = .{
            .name = s.fieldName(),
            .type = ?s.type,
            .default_value = &@as(?s.type, null),
            .is_comptime = false,
            .alignment = @alignOf(?s.type),
        };
    }
    return @Type(.{
        .Struct = .{
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        },
    });
}

fn failCompilationBadType(comptime T: type) noreturn {
    @compileError(std.fmt.comptimePrint(
        "Argument must be an integer, float, bool or slice of u8, got {s}",
        .{@typeName(T)},
    ));
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArgIterator = std.process.ArgIterator;

const getTerminalSize = @import("util.zig").getTerminalSize;

test {
    _ = std.testing.refAllDecls(@This());
    _ = std.testing.refAllDecls(CliCommand(
        "test",
        try std.SemanticVersion.parse("0.0.0"),
        .{ .parameters = &@import("main.zig").arg_spec },
    ));
}
