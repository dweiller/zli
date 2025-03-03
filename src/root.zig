const zli = @This();

pub const CliOptions = struct {
    parameters: []const Arg = &.{},
    subcommands: []const Command = &.{},
    help_message: []const u8 = &.{},
    include_help_option: bool = true,
    version: ?std.SemanticVersion = null,
};

pub const Command = struct {
    name: [:0]const u8,
    parameters: []const Arg = &.{},
    subcommands: []const Command = &.{},
    help_message: []const u8 = &.{},
    short_help: []const u8 = &.{},
    include_help_option: bool = true,
};

pub fn CliCommand(
    comptime name: []const u8,
    comptime options: CliOptions,
) type {
    const include_version = options.version != null;
    checkNameClash(options.parameters, options.include_help_option, include_version);
    for (options.subcommands) |s| {
        checkNameClash(s.parameters, s.include_help_option, false);
    }

    return struct {
        pub const ParsedResult = ParseResult(options.parameters, options.subcommands);
        pub const Params = ParsedResult.Params;

        pub const ParseError = error{PrintFailure} || Allocator.Error;

        pub fn parse(allocator: Allocator) ParseError!ParsedResult {
            var args_iter = try std.process.argsWithAllocator(allocator);
            defer args_iter.deinit();
            assert(args_iter.skip());
            return @This().parseWithIterator(allocator, &args_iter);
        }

        pub fn printHelp() std.fs.File.Writer.Error!void {
            try zli.printHelp(name, options);
        }

        pub fn printVersion() std.fs.File.Writer.Error!void {
            const writer = std.io.getStdOut().writer();
            try writeVersion(writer, name, options.version orelse unreachable);
        }

        pub fn parseWithIterator(
            allocator: Allocator,
            args_iter: anytype,
        ) ParseError!ParsedResult {
            const args = argsWithDefaults(
                options.parameters,
                options.include_help_option,
                include_version,
            );
            const subcommands = comptime blk: {
                var subcommands: [options.subcommands.len]Command = undefined;
                for (&subcommands, options.subcommands) |*sc, o| {
                    sc.* = o;
                    sc.parameters = argsWithDefaults(o.parameters, sc.include_help_option, false);
                }
                break :blk subcommands;
            };

            const parse_result =
                try zli.parseWithIterator(allocator, args, &subcommands, args_iter);

            switch (parse_result) {
                .ok => |parsed_args| {
                    if (parsed_args.options.help) {
                        @This().printHelp() catch return error.PrintFailure;
                        std.process.exit(0);
                    }

                    if (options.version) |_| {
                        if (parsed_args.options.version) {
                            printVersion() catch return error.PrintFailure;
                            std.process.exit(0);
                        }
                    }

                    const subcommand = if (options.subcommands.len == 0)
                        void{}
                    else if (parsed_args.subcommand) |sc| s: {
                        switch (sc) {
                            inline else => |opts, tag| {
                                const index = comptime for (options.subcommands, 0..) |s, i| {
                                    if (std.mem.eql(u8, s.name, @tagName(tag))) break i;
                                } else unreachable;
                                if (opts.help) {
                                    printSubcommandHelp(
                                        name ++ " " ++ @tagName(tag),
                                        index,
                                        options,
                                    ) catch return error.PrintFailure;
                                    std.process.exit(0);
                                }

                                var sub_opts: Options(options.subcommands[index].parameters) = undefined;
                                inline for (@typeInfo(@TypeOf(sub_opts)).@"struct".fields) |field| {
                                    @field(sub_opts, field.name) = @field(opts, field.name);
                                }
                                break :s @unionInit(ParsedResult.Params.CommandType, @tagName(tag), sub_opts);
                            },
                        }
                    } else null;

                    var opts: Options(options.parameters) = .{};
                    inline for (@typeInfo(@TypeOf(opts)).@"struct".fields) |field| {
                        @field(opts, field.name) = @field(parsed_args.options, field.name);
                    }

                    return .{ .ok = .{
                        .options = opts,
                        .subcommand = subcommand,
                        .positional = parsed_args.positional,
                    } };
                },
                .err => |err| return .{ .err = err },
            }
        }
    };
}

fn writeHelp(
    writer: anytype,
    columns: ?usize,
    name: []const u8,
    comptime options: CliOptions,
) !void {
    if (options.version) |version| {
        try writer.print("{s} {}\n", .{ name, version });
    } else {
        try writer.print("{s}\n", .{name});
    }
    try writer.writeAll(options.help_message);
    try writer.writeAll("\nOptions:\n\n");
    try writeOptions(
        writer,
        columns,
        40,
        60,
        argsWithDefaults(options.parameters, options.include_help_option, options.version != null),
    );
    if (options.subcommands.len > 0) {
        try writer.writeAll("\nSubcommands:\n\n");
        try writeSubcommands(
            writer,
            columns,
            40,
            60,
            options.subcommands,
        );
    }
}

pub fn printHelp(
    name: []const u8,
    comptime options: CliOptions,
) std.fs.File.Writer.Error!void {
    const stdout = std.io.getStdOut();
    const columns: ?usize = if (stdout.isTty())
        if (getTerminalSize()) |size|
            size.columns
        else
            null
    else
        null;

    try writeHelp(
        stdout.writer(),
        columns,
        name,
        options,
    );
}

pub fn printSubcommandHelp(
    name: []const u8,
    comptime subcommand_index: usize,
    comptime options: CliOptions,
) std.fs.File.Writer.Error!void {
    const stdout = std.io.getStdOut();
    const columns: ?usize = if (stdout.isTty())
        if (getTerminalSize()) |size|
            size.columns
        else
            null
    else
        null;

    const cli_opts: CliOptions = .{
        .parameters = options.subcommands[subcommand_index].parameters,
        .version = options.version,
    };

    try writeHelp(
        stdout.writer(),
        columns,
        name,
        cli_opts,
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
            length = @max(length, arg.fieldName().len);
        }
        break :length length;
    };

    const long_fmt = std.fmt.comptimePrint("{{s: <{d}}}", .{longest});

    const separator = " " ** 8;
    const indent_size = 2;
    const option_len_base = "  -h, --".len;
    const help_padding = indent_size + separator.len + longest + option_len_base;

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
        if (@typeInfo(arg.type) == .@"enum") {
            const pad = option_len_base + 4;
            try writer.writeAll(" " ** pad ++ "Options:\n");
            inline for (@typeInfo(arg.type).@"enum".fields) |field| {
                const enum_option_fmt = " " ** (pad + 2) ++ "{s}\n";
                try writer.print(enum_option_fmt, .{field.name});
            }
        }
    }
}

pub fn writeSubcommands(
    writer: anytype,
    columns: ?usize,
    min_width: usize,
    max_col_width: usize,
    comptime subcommands: []const Command,
) !void {
    const longest_name = comptime length: {
        var length: usize = 0;
        for (subcommands) |sc| {
            length = @max(length, sc.name.len);
        }
        break :length length;
    };

    const separator = " " ** 8;
    const indent_size = 2;
    const help_padding = indent_size + separator.len + longest_name;

    const max_width = if (columns) |width| max: {
        if (width < min_width + help_padding) break :max null;
        break :max @min(width, help_padding + max_col_width);
    } else null;

    const name_fmt = std.fmt.comptimePrint("{{s: <{d}}}", .{longest_name});

    inline for (subcommands) |sc| {
        if (max_width) |width| {
            try writer.print(name_fmt ++ separator, .{sc.name});
            try writeAlignedTo(writer, help_padding, width, sc.short_help);
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
) !void {
    try writer.print("{s} {}\n", .{ cannonical_name, version });
}

pub fn ParseResult(
    comptime spec: []const Arg,
    comptime subcommands: []const Command,
) type {
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
            pub const CommandType = CommandOptions(subcommands);
            options: Options(spec) = .{},
            subcommand: if (subcommands.len == 0) void else ?CommandType =
                if (subcommands.len == 0) void{} else null,
            positional: []const [:0]const u8 = &.{},

            pub fn deinit(self: Params, allocator: Allocator) void {
                inline for (std.meta.fields(@TypeOf(self.options))) |field| {
                    if (field.type == bool) continue;
                    const Child = @typeInfo(field.type).optional.child;
                    if (@typeInfo(Child) == .pointer) {
                        if (@field(self.options, field.name)) |slice| {
                            allocator.free(slice);
                        }
                    }
                }
                for (self.positional) |arg| allocator.free(arg);
                allocator.free(self.positional);

                if (subcommands.len == 0) return;

                if (self.subcommand) |sub| {
                    deinitSubcommand(allocator, sub);
                }
            }

            fn deinitSubcommand(allocator: Allocator, sub: CommandType) void {
                const active = std.meta.activeTag(sub);
                inline for (subcommands) |command| {
                    if (std.mem.eql(u8, command.name, @tagName(active))) {
                        inline for (std.meta.fields(Options(command.parameters))) |field| {
                            if (field.type == bool) continue;
                            const Child = @typeInfo(field.type).optional.child;
                            if (@typeInfo(Child) == .pointer) {
                                if (@field(@field(sub, command.name), field.name)) |slice| {
                                    allocator.free(slice);
                                }
                            }
                        }
                    }
                }
            }
        };
    };
}

pub const ParseErr = struct {
    arg_name: []const u8,
    string: []const u8,
    err: Error,

    pub fn deinit(self: ParseErr, allocator: Allocator) void {
        allocator.free(self.string);
    }

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

fn parseArg(
    comptime args: []const Arg,
    allocator: Allocator,
    options: *Options(args),
    args_iter: anytype,
    arg: []const u8,
) !?ParseErr {
    assert(arg.len > 1);
    assert(arg[0] == '-');
    assert(!std.mem.eql(u8, arg, "--"));

    if (arg[1] != '-') {
        for (arg[1 .. arg.len - 1]) |arg_char| {
            inline for (args) |s| {
                if (s.type == bool) {
                    if (s.name.matchesShort(arg_char)) {
                        @field(options, s.fieldName()) = true;
                        break;
                    }
                } else {
                    if (s.name.matchesShort(arg_char)) {
                        return .{
                            .arg_name = s.paramName(),
                            .string = try allocator.dupe(u8, arg),
                            .err = Error.NotLastShort,
                        };
                    }
                }
            } else return .{
                .arg_name = "",
                .string = try allocator.dupe(u8, arg),
                .err = Error.Unrecognized,
            };
        }
        const arg_char = arg[arg.len - 1];
        inline for (args) |s| {
            if (s.name.matchesShort(arg_char)) {
                switch (try parseArgValue(s, allocator, null, args_iter)) {
                    .ok => |v| {
                        if (comptime @typeInfo(s.type) == .pointer) {
                            if (@field(options, s.fieldName())) |slice| {
                                allocator.free(slice);
                            }
                        }
                        @field(options, s.fieldName()) = v;
                        break;
                    },
                    .err => |e| return e,
                }
            }
        } else return .{
            .arg_name = "",
            .string = try allocator.dupe(u8, arg),
            .err = Error.Unrecognized,
        };
    } else if (arg.len > 2 and arg[1] == '-') {
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
                        .ok => |v| {
                            if (comptime @typeInfo(s.type) == .pointer) {
                                if (@field(options, s.fieldName())) |slice| {
                                    allocator.free(slice);
                                }
                            }
                            @field(options, s.fieldName()) = v;
                        },
                        .err => |e| return e,
                    }
                    break;
                },
                .no => {},
            }
        } else return .{
            .arg_name = "",
            .string = try allocator.dupe(u8, arg),
            .err = Error.Unrecognized,
        };
    }
    return null;
}

fn parseArgValue(
    comptime s: Arg,
    allocator: Allocator,
    value_opt: ?[]const u8,
    args_iter: anytype,
) !union(enum) {
    ok: s.type,
    err: ParseErr,
} {
    const v = switch (@typeInfo(s.type)) {
        .int, .float, .pointer, .@"enum" => v: {
            const value = value_opt orelse args_iter.next() orelse return .{
                .err = .{
                    .arg_name = s.paramName(),
                    .string = "",
                    .err = Error.Missing,
                },
            };
            break :v switch (@typeInfo(s.type)) {
                .int => std.fmt.parseInt(s.type, value, 0) catch |err| return .{
                    .err = .{
                        .arg_name = s.paramName(),
                        .string = try allocator.dupe(u8, value),
                        .err = err,
                    },
                },
                .float => std.fmt.parseFloat(s.type, value) catch |err| return .{
                    .err = .{
                        .arg_name = s.paramName(),
                        .string = try allocator.dupe(u8, value),
                        .err = err,
                    },
                },
                .pointer => |p| switch (p.size) {
                    .one, .many, .c => failCompilationBadType(s.type),
                    .slice => slice: {
                        if (p.child != u8) failCompilationBadType(s.type);
                        if (p.sentinel()) |sentinel_ptr| {
                            const sentinel = @as(*align(1) const p.child, @ptrCast(sentinel_ptr)).*;
                            const copy = try allocator.alloc(p.child, value.len + 1);
                            @memcpy(copy[0..value.len], value);
                            copy[value.len] == sentinel;
                            break :slice copy[0..value.len :sentinel];
                        }
                        break :slice try allocator.dupe(u8, value);
                    },
                },
                .@"enum" => std.meta.stringToEnum(s.type, value) orelse return .{
                    .err = .{
                        .arg_name = s.paramName(),
                        .string = try allocator.dupe(u8, value),
                        .err = Error.BadValue,
                    },
                },
                else => unreachable,
            };
        },
        .bool => true,
        else => failCompilationBadType(s.type),
    };
    return .{ .ok = v };
}

pub fn parseWithIterator(
    allocator: Allocator,
    comptime args: []const Arg,
    comptime subcommands: []const Command,
    args_iter: anytype,
) !ParseResult(args, subcommands) {
    var options: Options(args) = .{};
    errdefer {
        inline for (args) |s| {
            if (comptime @typeInfo(s.type) == .pointer) {
                if (@field(options, s.fieldName())) |slice| {
                    allocator.free(slice);
                }
            }
        }
    }

    const p = while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--")) break arg;

        if (arg.len > 1 and arg[0] == '-') {
            if (try parseArg(args, allocator, &options, args_iter, arg)) |e| {
                inline for (args) |s| {
                    if (comptime @typeInfo(s.type) == .pointer) {
                        if (@field(options, s.fieldName())) |slice| {
                            allocator.free(slice);
                        }
                    }
                }
                return .{ .err = e };
            }
        } else {
            inline for (subcommands) |s| {
                if (std.mem.eql(u8, arg, s.name)) {
                    const subsubcommands = comptime blk: {
                        var subsubcommands: [s.subcommands.len]Command = undefined;
                        for (&subsubcommands, s.subcommands) |*sc, o| {
                            sc.* = o;
                            sc.parameters = argsWithDefaults(o.parameters, o.include_help_option, false);
                        }
                        break :blk subsubcommands;
                    };
                    const subopts = switch (try parseWithIterator(
                        allocator,
                        s.parameters,
                        &subsubcommands,
                        args_iter,
                    )) {
                        .ok => |o| o,
                        .err => |err| return .{ .err = err },
                    };

                    return .{ .ok = .{
                        .options = options,
                        .subcommand = @unionInit(CommandOptions(subcommands), s.name, subopts.options),
                        .positional = subopts.positional,
                    } };
                }
            } else break arg;
        }
    } else "--";

    var positional = std.ArrayList([:0]u8).init(allocator);
    errdefer {
        for (positional.items) |item| {
            allocator.free(item);
        }
        positional.deinit();
    }

    {
        var arg = p;
        while (!std.mem.eql(u8, arg, "--")) : (arg = args_iter.next() orelse break) {
            if (arg.len > 1 and arg[0] == '-') {
                if (try parseArg(args, allocator, &options, args_iter, arg)) |e| {
                    for (positional.items) |item| {
                        allocator.free(item);
                    }
                    positional.deinit();
                    inline for (args) |s| {
                        if (comptime @typeInfo(s.type) == .pointer) {
                            if (@field(options, s.fieldName())) |slice| {
                                allocator.free(slice);
                            }
                        }
                    }
                    return .{ .err = e };
                }
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
    allocator: Allocator,
    comptime args: []const Arg,
    comptime subcommands: []const Command,
) Allocator.Error!ParseResult(args, subcommands) {
    var args_iter = try std.process.argsWithAllocator(allocator);
    defer args_iter.deinit();
    assert(args_iter.skip());
    return parseWithIterator(allocator, args, subcommands, &args_iter);
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

fn checkNameClash(
    comptime args: []const Arg,
    comptime check_help: bool,
    comptime check_version: bool,
) void {
    for (args, 0..) |arg, i| {
        if (arg.name == .long and arg.name.long.full.len == 1) {
            @compileError("A long argument's full name must have length greater than 1");
        }
        switch (@typeInfo(arg.type)) {
            .int, .float, .bool, .@"enum" => {},
            .pointer => |p| {
                switch (p.size) {
                    .one, .many, .c => failCompilationBadType(arg.type),
                    .slice => if (p.child != u8) failCompilationBadType(arg.type),
                }
            },
            else => failCompilationBadType(arg.type),
        }
        for (args[i + 1 ..], 0..) |t, j| {
            if (arg.name.hasClash(t.name)) {
                @compileError(std.fmt.comptimePrint("arguments {d} and {d} have a name clash", .{ i, j }));
            }
        }

        if (check_help) {
            if (arg.name.hasClash(default_help_arg.name)) {
                @compileError(std.fmt.comptimePrint(
                    "argument {s} has a name clash with default parameter {s}",
                    .{ arg.fieldName(), default_help_arg.fieldName() },
                ));
            }
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
            .default_value_ptr = if (is_bool) &false else &@as(?s.type, null),
            .is_comptime = false,
            .alignment = @alignOf(?s.type),
        };
    }
    return @Type(.{
        .@"struct" = .{
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        },
    });
}

pub fn CommandOptions(comptime subcommands: []const Command) type {
    var tag_fields: [subcommands.len]std.builtin.Type.EnumField = undefined;
    var fields: [subcommands.len]std.builtin.Type.UnionField = undefined;
    for (&fields, &tag_fields, subcommands, 0..) |*f, *t, s, i| {
        t.* = .{
            .name = s.name,
            .value = i,
        };
        f.* = .{
            .name = s.name,
            .type = Options(s.parameters),
            .alignment = @alignOf(Options(s.parameters)),
        };
    }

    const Tag = @Type(.{
        .@"enum" = .{
            .tag_type = std.math.IntFittingRange(0, subcommands.len -| 1),
            .fields = &tag_fields,
            .decls = &.{},
            .is_exhaustive = true,
        },
    });

    return @Type(.{
        .@"union" = .{
            .layout = .auto,
            .tag_type = Tag,
            .fields = &fields,
            .decls = &.{},
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

comptime {
    if (@import("builtin").is_test) {
        _ = @import("test.zig");
    }
}
