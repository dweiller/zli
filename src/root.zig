const zli = @This();

pub const CliOptions = struct {
    parameters: []const Arg = &.{},
    help_message: []const u8 = &.{},
    version: ?std.SemanticVersion = null,
};

pub fn CliCommand(
    comptime name: []const u8,
    comptime options: CliOptions,
) type {
    const include_version = options.version != null;
    checkNameClash(options.parameters, include_version);

    return struct {
        pub const ParsedResult = ParseResult(options.parameters);
        pub const Params = ParsedResult.Params;

        pub fn parse(allocator: Allocator) Allocator.Error!ParsedResult {
            var args_iter = try std.process.argsWithAllocator(allocator);
            defer args_iter.deinit();
            assert(args_iter.skip());
            return @This().parseWithArgs(allocator, &args_iter);
        }

        pub fn parseOrExit(allocator: Allocator, status: u8) ParsedResult {
            return @This().parse(allocator) catch |err| {
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
            const args = argsWithDefaults(
                options.parameters,
                true,
                include_version,
            );

            switch (try zli.parseWithArgs(allocator, args, args_iter)) {
                .ok => |parsed_args| {
                    if (parsed_args.options.help) {
                        printHelpToStdout(
                            name,
                            options,
                            true,
                        ) catch std.process.exit(1);
                        std.process.exit(0);
                    }

                    if (options.version) |version| {
                        if (parsed_args.options.version) {
                            const writer = std.io.getStdOut().writer();
                            writeVersion(writer, name, version, "") catch std.process.exit(1);
                            std.process.exit(0);
                        }
                    }

                    var opts: Options(options.parameters) = .{};
                    inline for (@typeInfo(@TypeOf(opts)).Struct.fields) |field| {
                        @field(opts, field.name) = @field(parsed_args.options, field.name);
                    }

                    return .{ .ok = .{ .options = opts, .positional = parsed_args.positional } };
                },
                .err => |err| return .{ .err = err },
            }
        }
    };
}

fn printHelp(
    writer: anytype,
    columns: ?usize,
    name: []const u8,
    comptime options: CliOptions,
    comptime include_help: bool,
) !void {
    if (options.version) |version| {
        try writer.print("{s} {}\n", .{ name, version });
    } else {
        try writer.print("{s}\n", .{name});
    }
    if (options.help_message.len > 0)
        try writer.writerAll(options.help_message);
    try writer.writeAll("\nOptions:\n\n");
    try writeOptions(
        writer,
        columns,
        40,
        60,
        argsWithDefaults(options.parameters, include_help, options.version != null),
    );
}

pub fn printHelpToStdout(
    name: []const u8,
    comptime options: CliOptions,
    comptime include_help: bool,
) std.fs.File.Writer.Error!void {
    const stdout = std.io.getStdOut();
    const columns: ?usize = if (stdout.isTty())
        if (getTerminalSize()) |size|
            size.columns
        else
            null
    else
        null;

    try printHelp(
        stdout.writer(),
        columns,
        name,
        options,
        include_help,
    );
}

const default_help_arg: Arg = .{
    .name = .{ .long = .{ .full = "help" } },
    .short_help = "print help information",
    .type = bool,
};

const default_version_arg: Arg = .{
    .name = .{ .long = .{ .full = "version" } },
    .short_help = "print version information",
    .type = bool,
};

pub fn argsWithDefaults(
    comptime args: []const Arg,
    comptime include_help: bool,
    comptime include_version: bool,
) []const Arg {
    return args ++
        (if (include_help) .{default_help_arg} else .{}) ++
        (if (include_version) .{default_version_arg} else .{});
}

pub fn writeOptions(
    writer: anytype,
    columns: ?usize,
    min_width: usize,
    max_col_width: usize,
    comptime spec: []const Arg,
) !void {
    const longest = comptime length: {
        var length = 0;
        for (spec) |arg| {
            const arg_name = arg.fieldName();
            if (arg_name.len > length) {
                length = arg_name.len;
            }
        }
        break :length length;
    };

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
    assert(end > padding);

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
        err: ParseErr,

        pub fn deinit(self: @This(), allocator: Allocator) void {
            switch (self) {
                .ok => |ok| ok.deinit(allocator),
                .err => |err| allocator.free(err.string),
            }
        }

        pub const Params = struct {
            options: Options(spec),
            positional: []const [:0]const u8,

            pub fn deinit(self: Params, allocator: Allocator) void {
                inline for (std.meta.fields(@TypeOf(self.options))) |field| {
                    if (field.type == bool) continue;
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
    };
}

pub const ParseErr = struct {
    arg_name: []const u8,
    string: []const u8,
    err: Error,

    pub fn renderToStdErr(value: ParseErr) void {
        value.render(std.io.getStdErr().writer()) catch {};
    }

    pub fn render(value: ParseErr, writer: anytype) !void {
        switch (value.err) {
            error.Missing => try writer.print(
                "missing value for parameter {s}\n",
                .{value.arg_name},
            ),
            error.BadValue => try writer.print(
                "invalid value for parameter {s}\n",
                .{value.arg_name},
            ),
            error.Unrecognized => try writer.print(
                "unrecognized parameter '{s}'\n",
                .{value.string},
            ),
            error.NotLastShort => try writer.print(
                \\invalid concatenation of short options: {s}
                \\when given as a short option, parameter {s} must appear last in a concatenation of short options
                \\
            ,
                .{ value.string, value.arg_name },
            ),
            error.InvalidCharacter => try writer.print(
                "value '{s}' for parameter {s} contains an invalid character\n",
                .{ value.string, value.arg_name },
            ),
            error.Overflow => try writer.print(
                "value '{s}' for parameter {s} is out of bounds (try something closer to 0)\n",
                .{ value.string, value.arg_name },
            ),
        }
    }
};

pub const Error = error{ Missing, BadValue, Unrecognized, NotLastShort } ||
    std.fmt.ParseIntError || std.fmt.ParseFloatError;

pub fn parseWithArgs(
    allocator: Allocator,
    comptime args: []const Arg,
    args_iter: *std.process.ArgIterator,
) !ParseResult(args) {
    var options: Options(args) = .{};
    var positional = std.ArrayList([:0]u8).init(allocator);

    while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--")) break;

        if (arg.len > 1 and arg[0] == '-' and arg[1] != '-') {
            inline for (args) |s| {
                for (arg[1 .. arg.len - 1]) |arg_char| {
                    if (s.type == bool) {
                        if (s.name.matchesShort(arg_char)) {
                            @field(options, s.fieldName()) = true;
                        }
                    } else {
                        if (s.name.matchesShort(arg_char)) {
                            return .{
                                .err = .{
                                    .arg_name = s.paramName(),
                                    .string = try allocator.dupe(u8, arg),
                                    .err = Error.NotLastShort,
                                },
                            };
                        }
                    }
                }
                const arg_char = arg[arg.len - 1];
                if (s.name.matchesShort(arg_char)) {
                    switch (try parseArgValue(s, allocator, null, args_iter)) {
                        .ok => |v| @field(options, s.fieldName()) = v,
                        .err => |e| return .{ .err = e },
                    }
                }
            }
        } else if (arg.len > 2 and std.mem.startsWith(u8, arg, "--")) {
            inline for (args) |s| {
                if (s.name == .short) continue;
                const match = s.name.matchesLong(arg[2..]);
                switch (match) {
                    .yes, .long_with_eql => |tag| {
                        const embedded_value = if (tag == .long_with_eql)
                            arg[2 + s.name.long.full.len + 1 ..]
                        else
                            null;

                        switch (try parseArgValue(s, allocator, embedded_value, args_iter)) {
                            .ok => |v| @field(options, s.fieldName()) = v,
                            .err => |e| return .{ .err = e },
                        }
                        break;
                    },
                    .no => {},
                }
            }
        } else {
            try positional.append(try allocator.dupeZ(u8, arg));
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

fn parseArgValue(
    comptime s: Arg,
    allocator: Allocator,
    value_opt: ?[]const u8,
    args_iter: *std.process.ArgIterator,
) !union(enum) {
    ok: s.type,
    err: ParseErr,
} {
    const v = switch (@typeInfo(s.type)) {
        .Int, .Float, .Pointer, .Enum => v: {
            const value = value_opt orelse args_iter.next() orelse return .{
                .err = .{
                    .arg_name = s.paramName(),
                    .string = "",
                    .err = Error.Missing,
                },
            };
            break :v switch (@typeInfo(s.type)) {
                .Int => std.fmt.parseInt(s.type, value, 0) catch |err| return .{
                    .err = .{
                        .arg_name = s.paramName(),
                        .string = try allocator.dupe(u8, value),
                        .err = err,
                    },
                },
                .Float => std.fmt.parseFloat(s.type, value) catch |err| return .{
                    .err = .{
                        .arg_name = s.paramName(),
                        .string = try allocator.dupe(u8, value),
                        .err = err,
                    },
                },
                .Pointer => |p| switch (p.size) {
                    .One, .Many, .C => failCompilationBadType(s.type),
                    .Slice => slice: {
                        if (p.child != u8) failCompilationBadType(s.type);
                        if (p.sentinel) |sentinel_ptr| {
                            const sentinel = @as(*align(1) const p.child, @ptrCast(sentinel_ptr)).*;
                            const copy = try allocator.alloc(p.child, value.len + 1);
                            @memcpy(copy[0..value.len], value);
                            copy[value.len] == sentinel;
                            break :slice copy[0..value.len :sentinel];
                        }
                        break :slice try allocator.dupe(u8, value);
                    },
                },
                .Enum => std.meta.stringToEnum(s.type, value) orelse return .{
                    .err = .{
                        .arg_name = s.paramName(),
                        .string = try allocator.dupe(u8, value),
                        .err = Error.BadValue,
                    },
                },
                else => unreachable,
            };
        },
        .Bool => true,
        else => failCompilationBadType(s.type),
    };
    return .{ .ok = v };
}

pub fn parse(allocator: Allocator, comptime args: []const Arg) Allocator.Error!ParseResult(args) {
    var args_iter = try std.process.argsWithAllocator(allocator);
    defer args_iter.deinit();
    assert(args_iter.skip());
    return parseWithArgs(allocator, args, &args_iter);
}

pub const ArgName = union(enum) {
    long: struct { full: [:0]const u8, short: ?u8 = null },
    short: u8,

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

fn checkNameClash(comptime args: []const Arg, comptime check_version: bool) void {
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

        if (arg.name.hasClash(default_help_arg.name)) {
            @compileError(std.fmt.comptimePrint(
                "argument {s} has a name clash with default parameter {s}",
                .{ arg.fieldName(), default_help_arg.fieldName() },
            ));
        }

        if (check_version) {
            if (arg.name.hasClash(default_version_arg.name)) {
                @compileError(std.fmt.comptimePrint(
                    "argument {s} has a name clash with default parameter {s}",
                    .{ arg.fieldName(), default_version_arg.fieldName() },
                ));
            }
        }
    }
}

pub fn Options(comptime args: []const Arg) type {
    var fields: [args.len]std.builtin.Type.StructField = undefined;
    for (&fields, args) |*f, s| {
        const is_bool = s.type == bool;
        f.* = .{
            .name = s.fieldName(),
            .type = if (is_bool) bool else ?s.type,
            .default_value = if (is_bool) &false else &@as(?s.type, null),
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
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArgIterator = std.process.ArgIterator;

const getTerminalSize = @import("util.zig").getTerminalSize;

test {
    _ = std.testing.refAllDecls(@This());
    _ = std.testing.refAllDecls(CliCommand(
        "test",
        .{
            .parameters = &@import("main.zig").arg_spec,
            .version = try std.SemanticVersion.parse("0.0.0"),
        },
    ));
}
