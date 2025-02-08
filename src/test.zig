const zli = @import("root.zig");
const std = @import("std");
const assert = std.debug.assert;

const arg_spec: []const zli.Arg = &.{
    .{
        .name = .{ .long = .{ .full = "long-short", .short = 'l' } },
        .short_help = "long-short option",
        .type = []const u8,
    },
    .{
        .name = .{ .short = 's' },
        .short_help = "short option",
        .type = u8,
    },
    .{
        .name = .{ .long = .{ .full = "enum-option" } },
        .short_help = "enum-valued option",
        .type = enum { yes, no },
    },
    .{
        .name = .{ .long = .{ .full = "flag-1", .short = '1' } },
        .short_help = "long-short flag",
        .type = bool,
    },
    .{
        .name = .{ .short = '2' },
        .short_help = "short flag",
        .type = bool,
    },
    .{
        .name = .{ .long = .{ .full = "flag-3" } },
        .short_help = "long flag",
        .type = bool,
    },
};

const sub_arg_spec: []const zli.Arg = &.{
    .{
        .name = .{ .long = .{ .full = "s-long-short", .short = 'l' } },
        .short_help = "long-short option",
        .type = []const u8,
    },
    .{
        .name = .{ .short = 't' },
        .short_help = "short option",
        .type = u8,
    },
    .{
        .name = .{ .long = .{ .full = "s-enum-option" } },
        .short_help = "enum-valued option",
        .type = enum { yes, no },
    },
    .{
        .name = .{ .long = .{ .full = "s-flag-1", .short = '1' } },
        .short_help = "long-short flag",
        .type = bool,
    },
    .{
        .name = .{ .short = '3' },
        .short_help = "short flag",
        .type = bool,
    },
    .{
        .name = .{ .long = .{ .full = "s-flag-3" } },
        .short_help = "long flag",
        .type = bool,
    },
};

const sub_spec: []const zli.Command = &.{
    .{
        .name = "no-opts",
    },
    .{
        .name = "has-opts",
        .parameters = sub_arg_spec,
    },
};

const Cli = zli.CliCommand("test-cli", .{ .parameters = arg_spec });
const SubCli = zli.CliCommand("test-sub-cli", .{ .parameters = arg_spec, .subcommands = sub_spec });

test "single long option" {
    var argv = std.ArrayList(u8).init(std.testing.allocator);
    defer argv.deinit();

    inline for (arg_spec) |a| {
        if (a.name != .long) continue;
        argv.clearRetainingCapacity();
        try argv.writer().print("--{s}", .{a.name.long.full});
        try argv.append(' ');
        if (genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
        }

        var expected: zli.Options(arg_spec) = .{};
        @field(expected, a.fieldName()) = genArgValue(a.type);

        try check(argv.items, expected, &.{});
    }

    inline for (sub_arg_spec) |a| {
        if (a.name != .long) continue;
        argv.clearRetainingCapacity();
        try argv.appendSlice("has-opts ");
        try argv.writer().print("--{s}", .{a.name.long.full});
        try argv.append(' ');
        if (genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
        }

        var expected: zli.Options(sub_arg_spec) = .{};
        @field(expected, a.fieldName()) = genArgValue(a.type);

        try checkSub(argv.items, .{}, .{ .@"has-opts" = expected }, &.{});
    }
}

test "all long options" {
    var argv = std.ArrayList(u8).init(std.testing.allocator);
    defer argv.deinit();

    var expected: zli.Options(arg_spec) = .{};

    inline for (arg_spec) |a| {
        if (a.name != .long) continue;
        try argv.writer().print("--{s}", .{a.name.long.full});
        try argv.append(' ');
        if (comptime genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
            @field(expected, a.fieldName()) = genArgValue(a.type);
        } else {
            @field(expected, a.fieldName()) = true;
        }
    }

    try check(argv.items, expected, &.{});

    try argv.appendSlice("has-opts ");

    var sub_expected: zli.Options(sub_arg_spec) = .{};

    inline for (sub_arg_spec) |a| {
        if (a.name != .long) continue;
        try argv.writer().print("--{s}", .{a.name.long.full});
        try argv.append(' ');
        if (comptime genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
            @field(sub_expected, a.fieldName()) = genArgValue(a.type);
        } else {
            @field(sub_expected, a.fieldName()) = true;
        }
    }

    try checkSub(argv.items, expected, .{ .@"has-opts" = sub_expected }, &.{});
}

test "single short option" {
    var argv = std.ArrayList(u8).init(std.testing.allocator);
    defer argv.deinit();

    inline for (arg_spec) |a| {
        const short = switch (a.name) {
            .long => |n| if (n.short) |s| s else continue,
            .short => |s| s,
        };
        argv.clearRetainingCapacity();
        try argv.writer().print("-{c}", .{short});
        try argv.append(' ');
        if (genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
        }

        var expected: zli.Options(arg_spec) = .{};
        @field(expected, a.fieldName()) = genArgValue(a.type);

        try check(argv.items, expected, &.{});
    }

    inline for (sub_arg_spec) |a| {
        const short = switch (a.name) {
            .long => |n| if (n.short) |s| s else continue,
            .short => |s| s,
        };
        argv.clearRetainingCapacity();
        try argv.appendSlice("has-opts ");
        try argv.writer().print("-{c}", .{short});
        try argv.append(' ');
        if (genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
        }

        var expected: zli.Options(sub_arg_spec) = .{};
        @field(expected, a.fieldName()) = genArgValue(a.type);

        try checkSub(argv.items, .{}, .{ .@"has-opts" = expected }, &.{});
    }
}

test "all short options" {
    var argv = std.ArrayList(u8).init(std.testing.allocator);
    defer argv.deinit();

    var expected: zli.Options(arg_spec) = .{};

    inline for (arg_spec) |a| {
        const short = switch (a.name) {
            .long => |n| if (n.short) |s| s else continue,
            .short => |s| s,
        };
        try argv.writer().print("-{c}", .{short});
        try argv.append(' ');
        if (comptime genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
            @field(expected, a.fieldName()) = genArgValue(a.type);
        } else {
            @field(expected, a.fieldName()) = true;
        }
    }

    try check(argv.items, expected, &.{});

    try argv.appendSlice("has-opts ");

    var sub_expected: zli.Options(sub_arg_spec) = .{};

    inline for (sub_arg_spec) |a| {
        const short = switch (a.name) {
            .long => |n| if (n.short) |s| s else continue,
            .short => |s| s,
        };
        try argv.writer().print("-{c}", .{short});
        try argv.append(' ');
        if (comptime genCliArgValue(a.type)) |v| {
            try argv.appendSlice(v);
            try argv.append(' ');
            @field(sub_expected, a.fieldName()) = genArgValue(a.type);
        } else {
            @field(sub_expected, a.fieldName()) = true;
        }
    }

    try checkSub(argv.items, expected, .{ .@"has-opts" = sub_expected }, &.{});
}

test "clustered flags" {
    try check("-12", .{ .@"flag-1" = true, .@"2" = true }, &.{});
    try check("-21l final-flag-arg", .{
        .@"long-short" = "final-flag-arg",
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("-1s 8 -2l final-flag-arg", .{
        .@"long-short" = "final-flag-arg",
        .s = 8,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});

    try checkSub("has-opts -13", .{}, .{ .@"has-opts" = .{ .@"s-flag-1" = true, .@"3" = true } }, &.{});
    try checkSub("has-opts -31l final-flag-arg", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "final-flag-arg",
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts -1t 8 -3l final-flag-arg", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "final-flag-arg",
        .t = 8,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
}

test "long and short options" {
    try check("-1 --long-short long-string-arg", .{
        .@"long-short" = "long-string-arg",
        .@"flag-1" = true,
    }, &.{});
    try check("-1 --long-short long-string-arg --flag-3", .{
        .@"long-short" = "long-string-arg",
        .@"flag-1" = true,
        .@"flag-3" = true,
    }, &.{});
    try check("--long-short long-string-arg --flag-3 -1", .{
        .@"long-short" = "long-string-arg",
        .@"flag-1" = true,
        .@"flag-3" = true,
    }, &.{});

    try checkSub("has-opts -1 --s-long-short long-string-arg", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "long-string-arg",
        .@"s-flag-1" = true,
    } }, &.{});
    try checkSub("has-opts -1 --s-long-short long-string-arg --s-flag-3", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "long-string-arg",
        .@"s-flag-1" = true,
        .@"s-flag-3" = true,
    } }, &.{});
    try checkSub("has-opts --s-long-short long-string-arg --s-flag-3 -1", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "long-string-arg",
        .@"s-flag-1" = true,
        .@"s-flag-3" = true,
    } }, &.{});
}

test "clustered and long" {
    try check("-12 --enum-option yes", .{
        .@"enum-option" = .yes,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("--enum-option yes -12", .{
        .@"enum-option" = .yes,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("--enum-option yes -21l final-flag-arg", .{
        .@"long-short" = "final-flag-arg",
        .@"enum-option" = .yes,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("-1s 8 --enum-option yes -2l final-flag-arg", .{
        .@"long-short" = "final-flag-arg",
        .s = 8,
        .@"enum-option" = .yes,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("--enum-option yes -1s 8 -2l final-flag-arg", .{
        .@"long-short" = "final-flag-arg",
        .s = 8,
        .@"enum-option" = .yes,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});

    try checkSub("has-opts -13 --s-enum-option yes", .{}, .{ .@"has-opts" = .{
        .@"s-enum-option" = .yes,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts --s-enum-option yes -13", .{}, .{ .@"has-opts" = .{
        .@"s-enum-option" = .yes,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts --s-enum-option yes -31l final-flag-arg", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "final-flag-arg",
        .@"s-enum-option" = .yes,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts -1t 8 --s-enum-option yes -3l final-flag-arg", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "final-flag-arg",
        .t = 8,
        .@"s-enum-option" = .yes,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts --s-enum-option yes -1t 8 -3l final-flag-arg", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "final-flag-arg",
        .t = 8,
        .@"s-enum-option" = .yes,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
}

test "clustered and short" {
    try check("-12 -s 8", .{
        .s = 8,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("-s 8 -12", .{
        .s = 8,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("-21l final-flag-arg -s 8", .{
        .@"long-short" = "final-flag-arg",
        .s = 8,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});
    try check("-s 8 -21l final-flag-arg", .{
        .@"long-short" = "final-flag-arg",
        .s = 8,
        .@"flag-1" = true,
        .@"2" = true,
    }, &.{});

    try checkSub("has-opts -13 -t 8", .{}, .{ .@"has-opts" = .{
        .t = 8,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts -t 8 -13", .{}, .{ .@"has-opts" = .{
        .t = 8,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts -31l final-flag-arg -t 8", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "final-flag-arg",
        .t = 8,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
    try checkSub("has-opts -t 8 -31l final-flag-arg", .{}, .{ .@"has-opts" = .{
        .@"s-long-short" = "final-flag-arg",
        .t = 8,
        .@"s-flag-1" = true,
        .@"3" = true,
    } }, &.{});
}

test "option appearing multiple times" {
    try check("-1 -1", .{ .@"flag-1" = true }, &.{});
    try check("-11", .{ .@"flag-1" = true }, &.{});
    try check("-s 7 -s 8", .{ .s = 8 }, &.{});
    try check("-l arg-1 -l arg-2", .{ .@"long-short" = "arg-2" }, &.{});

    try checkSub("has-opts -1 -1", .{}, .{ .@"has-opts" = .{ .@"s-flag-1" = true } }, &.{});
    try checkSub("has-opts -11", .{}, .{ .@"has-opts" = .{ .@"s-flag-1" = true } }, &.{});
    try checkSub("has-opts -t 7 -t 8", .{}, .{ .@"has-opts" = .{ .t = 8 } }, &.{});
    try checkSub("has-opts -l arg-1 -l arg-2", .{}, .{ .@"has-opts" = .{ .@"s-long-short" = "arg-2" } }, &.{});
}

test "unrecognized option" {
    try std.testing.expectError(error.Unrecognized, check("-u", .{}, &.{}));
    try std.testing.expectError(error.Unrecognized, check("-u -1", .{ .@"flag-1" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("-1 -u", .{ .@"flag-1" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("-1u", .{ .@"flag-1" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("-u1", .{ .@"flag-1" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("--long-short long-string-arg -u", .{
        .@"long-short" = "long-string-arg",
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("-u --long-short long-string-arg", .{
        .@"long-short" = "long-string-arg",
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("--bad", .{}, &.{}));
    try std.testing.expectError(error.Unrecognized, check("--bad -1", .{ .@"flag-1" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("-1 --bad", .{ .@"flag-1" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("-12 --bad", .{ .@"flag-1" = true, .@"2" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("--bad -12", .{ .@"flag-1" = true, .@"2" = true }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("--long-short long-string-arg --bad", .{
        .@"long-short" = "long-string-arg",
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, check("--bad --long-short long-string-arg", .{
        .@"long-short" = "long-string-arg",
    }, &.{}));

    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -u", .{}, .{
        .@"has-opts" = .{},
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -u -1", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -1 -u", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -1u", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -u1", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts --s-long-short long-string-arg -u", .{}, .{
        .@"has-opts" = .{ .@"s-long-short" = "long-string-arg" },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -u --s-long-short long-string-arg", .{}, .{
        .@"has-opts" = .{ .@"s-long-short" = "long-string-arg" },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts --bad", .{}, .{ .@"has-opts" = .{} }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts --bad -1", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -1 --bad", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts -13 --bad", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true, .@"3" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts --bad -13", .{}, .{
        .@"has-opts" = .{ .@"s-flag-1" = true, .@"3" = true },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts --s-long-short long-string-arg --bad", .{}, .{
        .@"has-opts" = .{ .@"s-long-short" = "long-string-arg" },
    }, &.{}));
    try std.testing.expectError(error.Unrecognized, checkSub("has-opts --bad --s-long-short long-string-arg", .{}, .{
        .@"has-opts" = .{ .@"s-long-short" = "long-string-arg" },
    }, &.{}));
}

test "positional" {
    try check("a b c d", .{}, &.{ "a", "b", "c", "d" });
    try check("a -l arg b c d", .{ .@"long-short" = "arg" }, &.{ "a", "b", "c", "d" });
    try check("a -- -l arg b c d", .{}, &.{ "a", "-l", "arg", "b", "c", "d" });
}

fn check(argv: []const u8, expected: zli.Options(arg_spec), positional: []const []const u8) !void {
    var iter = std.mem.tokenizeScalar(u8, argv, ' ');

    const params = switch (try Cli.parseWithIterator(
        std.testing.allocator,
        &iter,
    )) {
        .ok => |v| v,
        .err => |e| {
            e.deinit(std.testing.allocator);
            return e.err;
        },
    };
    defer params.deinit(std.testing.allocator);

    try std.testing.expectEqualDeep(expected, params.options);
    try std.testing.expectEqualDeep(positional, params.positional);

    try checkSub(argv, expected, null, positional);
}

fn checkSub(
    argv: []const u8,
    expected: zli.Options(arg_spec),
    sub_expected: ?SubCli.ParsedResult.Params.CommandType,
    positional: []const []const u8,
) !void {
    var iter = std.mem.tokenizeScalar(u8, argv, ' ');

    const params = switch (try SubCli.parseWithIterator(
        std.testing.allocator,
        &iter,
    )) {
        .ok => |v| v,
        .err => |e| {
            e.deinit(std.testing.allocator);
            return e.err;
        },
    };
    defer params.deinit(std.testing.allocator);

    try std.testing.expectEqualDeep(expected, params.options);
    try std.testing.expectEqualDeep(positional, params.positional);
    try std.testing.expectEqualDeep(sub_expected, params.subcommand);
}

fn genCliArgValue(comptime T: type) ?[]const u8 {
    switch (@typeInfo(T)) {
        .int => return "12",
        .float => return "12.34",
        .@"enum" => |info| return info.fields[0].name,
        .bool => return null,
        .pointer => |info| {
            comptime assert(info.child == u8);
            comptime assert(info.size == .slice);
            return "a-string-arg";
        },
        else => unreachable,
    }
}

fn genArgValue(comptime T: type) T {
    switch (@typeInfo(T)) {
        .int => return 12,
        .float => return 12.34,
        .@"enum" => |info| return @enumFromInt(info.fields[0].value),
        .bool => return true,
        .pointer => |info| {
            comptime assert(info.child == u8);
            comptime assert(info.size == .slice);
            return "a-string-arg";
        },
        else => unreachable,
    }
}
