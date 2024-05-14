# zli

zli is a CLI argument parsing module for Zig. The aim is to provide CLI parameters in a simple, declarative way and have zli parse them as well as generate `--help` and `--version` flags and usage error messages.

## usage
A usage example is given by `src/main.zig`. CLI parameters are declared like so:
```zig
const arg_spec = [_]zli.Arg{
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
```
CLI parameters can then be parsed with `try Cli.parse(allocator)`.
The auto-generated help message generated by the example is:
```
zli-example-app 0.0.0

Options:

  -f, --file                     file name
  -a                             short only option
      --a-really-long-arg        How does this look?
  -k, --complicated              This option is very complicated and the 'short' help message
                                   included here is so long that we want some nice line
                                   breaking to occur
      --help                     Print this help message
      --version                  Print version information
```
which can you can see by running `zig build run -- --help`.

To use zli in a project you can use the Zig package manager by using `https://github.com/dweiller/zli/archive/[[COMMIT-SHA]].tar.gz` as the dependency URL and then grabbing the 'zli' module, e.g.:

add zli to your `build.zig.zon`:
```sh
zig fetch --save=zli https://github.com/dweiller/zli/archive/[[COMMIT-SHA]].tar.gz
```

`build.zig`:
```zig
pub fn build(b: *std.Build) void {

    // -- snip --

    const exe = ...; // your executable

    const zli = b.dependency("zli").module("zli");
    exe.root_module.addImport("zli", zli);

    // -- snip --
}
```
