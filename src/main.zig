pub const arg_spec = [_]zli.Arg{
    .{
        .name = .{ .long = .{ .full = "file", .short = 'f' } },
        .short_help = "file name",
        .type = []const u8,
    },
    .{
        .name = .{ .short = 'a' },
        .short_help = "short only option",
        .type = u8,
    },
    .{
        .name = .{ .long = .{ .full = "a-really-long-arg" } },
        .short_help = "How does this look?",
        .type = enum { yes, no },
    },
    .{
        .name = .{ .long = .{ .full = "complicated", .short = 'k' } },
        .short_help = "This option is very complicated and the 'short' help message included here is so long that we want some nice line breaking to occur",
    },
};

const version = std.SemanticVersion{
    .major = 0,
    .minor = 0,
    .patch = 0,
};

const Cli = zli.CliCommand("zli-example-app", .{ .parameters = &arg_spec, .version = version });

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const parse_result = Cli.parseOrExit(allocator, 3);
    defer parse_result.deinit(allocator);

    const params = switch (parse_result) {
        .ok => |ok| ok,
        .err => |err| {
            err.renderToStdErr();
            std.process.exit(1);
        },
    };

    _ = params;
}

const std = @import("std");

const zli = @import("root.zig");
