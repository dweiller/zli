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
    .{
        .name = .{ .long = .{ .full = "flag-1", .short = '1' } },
        .short_help = "just a flag",
        .type = bool,
    },
    .{
        .name = .{ .short = '2' },
        .short_help = "another flag",
        .type = bool,
    },
};

const version = std.SemanticVersion{
    .major = 0,
    .minor = 0,
    .patch = 0,
};

const Cli = zli.CliCommand("zli-example-app", .{
    .parameters = &arg_spec,
    .version = version,
    .subcommands = &.{
        .{
            .name = "sub1",
            .short_help = "this is a subcommand",
            .parameters = &.{
                .{
                    .name = .{ .short = 'b' },
                    .short_help = "subcommand parameters",
                    .type = bool,
                },
                .{
                    .name = .{ .long = .{ .full = "enum-option" } },
                    .short_help = "an option taking an enum",
                    .type = enum { a, ab, abc },
                },
            },
        },
        .{
            .name = "long-subcommand",
            .short_help = "this does nothing but here is a really long help message anyway in order to trigger line-breaking",
        },
        .{
            .name = "sub-without-help",
        },
    },
});

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const parse_result = Cli.parse(allocator) catch |err| {
        std.log.err("{s}", .{@errorName(err)});
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
        std.process.exit(3);
    };
    defer parse_result.deinit(allocator);

    const params = switch (parse_result) {
        .ok => |ok| ok,
        .err => |err| {
            err.renderToStdErr();
            std.process.exit(1);
        },
    };

    const stdout = std.io.getStdOut();
    const writer = stdout.writer();

    writer.print("parsed args: {}\n", .{params}) catch @panic("failed to write to stdout");
}

const std = @import("std");

const zli = @import("root.zig");
