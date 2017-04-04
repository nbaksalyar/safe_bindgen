**_This is a fork of rusty-cheddar until such time as
the [rusty-binder version](https://gitlab.com/rusty-binder/rusty-cheddar)
is available._**

# moz-cheddar

[![Build Status](https://travis-ci.org/mozilla/moz-cheddar.svg?branch=master)](https://travis-ci.org/mozilla/moz-cheddar)
[![crates.io](http://meritbadge.herokuapp.com/moz-cheddar)](https://crates.io/crates/moz-cheddar)
![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)

moz-cheddar is a library for converting Rust source files into C header files.

**A note on versioning:** While moz-cheddar is still pre-`v1.0.0` it will
likely go through numerous breaking changes. We attempt to follow semver
and bump the minor version any time a new feature is added or output
behavior is changed.

moz-cheddar targets C99 or later (for single line comments, `stdint.h` and
`stdbool.h`).

The most useful way to use moz-cheddar is in a build script.
To do this add the following `build-dependencies` section to your `Cargo.toml`
(to use it as a normal library simply replace `build-dependencies`
with `dependencies`):

```toml
# Cargo.toml

[build-dependencies]
moz-cheddar = "0.4.0"
```

Then create the following `build.rs`:

```rust
// build.rs

extern crate cheddar;

fn main() {
    cheddar::Cheddar::new().expect("could not read manifest")
        .run_build("include/my_header.h");
}
```

This should work as is providing you've set up your project correctly.
**Don't forget to add a `build = ...` to your `[package]` section,
see [the cargo docs] for more info.**

moz-cheddar will then create a `my_header.h` file in `include/`.
Note that moz-cheddar emits very few warnings; it is up to the
programmer to write a library which can be correctly called from C.

#### API In a Module

You can also place your API in a module to help keep your source code neat.
To do this you must supply the name of the module to Cheddar, then ensure
that the items are available in the top-level scope:

```rust
// build.rs

extern crate cheddar;

fn main() {
    cheddar::Cheddar::new().expect("could not read manifest")
        .module("c_api").expect("malformed module path")
        .run_build("target/include/rusty.h");
}
```

```rust
// src/lib.rs

pub use c_api::*;

mod c_api {
    // api goes here ...
}
```

There are also `.compile()` and `.compile_code()` methods for finer control.

## Conversions

In the examples below, boilerplate has been omitted from the header.

### Typedefs

moz-cheddar converts `pub type A = B` into `typedef B A;`.
Types containing generics are ignored.

Rust:

```rust
type UInt32 = u32;
pub type UInt64 = u64;
pub type MyOption<T> = Option<T>
```

Header:

```C
// Some boilerplate omitted.
typedef uint64_t UInt64;
// Some more boilerplate omitted.
```

### Enums

moz-cheddar will convert public enums which are marked `#[repr(C)]`.
If the enum is generic or contains tuple or struct variants then `cheddar`
will fail. moz-cheddar should correctly handle explicit discriminants.

Rust:

```rust
#[repr(C)]
pub enum Colours {
    Red = -6,
    Blue,
    Green = 7,
    Yellow,
}

// This would fail if it was #[repr(C)].
pub enum Tastes<T> {
    Savoury(T),
    Sweet,
}

// This would fail if it was public.
#[repr(C)]
enum Units {
    Kg(f64),
    M(f64),
    S(f64),
    A(f64),
    K(f64),
    Mol(f64),
    Cd(f64),
}
```

Header:

```C
// Some boilerplate omitted.
typedef enum Colours {
        Colours_Red = -6,
        Colours_Blue,
        Colours_Green = 7,
        Colours_Yellow,
} Colours;
// Some more boilerplate omitted.
```

### Structs

Structs are handled very similarly to enums, they must be public,
marked `#[repr(C)]`, and they must not contain generics.
This currently only checked at the struct-level.
Generic fields are not checked.

Rust:

```rust
#[repr(C)]
pub struct Person {
    age: i32,
    height: f64,
    weight: f64,
}
```

Header:

```C
// Some boilerplate omitted.
typedef struct Person {
        int32_t age;
        double height;
        double weight;
} Person;
// Some more boilerplate omitted.
```

#### Opaque Structs

One common C idiom is to hide the implementation of a struct using
an opaque struct, which can only be used behind a pointer.
This is especially useful in Rust-C interfaces as it allows you
to use _any arbitrary Rust struct_ in C.

To define an opaque struct you must define a public newtype which
is marked as `#[repr(C)]`.

Rust:

```rust
struct Foo<T> {
    bar: i32,
    baz: Option<T>,
}

#[repr(C)]
pub struct MyCrate_Foo(Foo<PathBuf>);
```

Header:

```C
// Some boilerplate omitted.
typedef struct MyCrate_Foo MyCrate_Foo;
// Some boilerplate omitted.
```

Note that the newtype _must not_ be generic but the type that
it wraps can be arbitrary.

### Functions

For moz-cheddar to pick up on a function declaration it must be public,
marked `#[no_mangle]` and have one of the following ABIs:

- C
- Cdecl
- Stdcall
- Fastcall
- System

If you believe one of these has been included in error, or if one has
been omitted, then please open an issue at the [repo].

moz-cheddar will fail on functions which are marked as diverging (`-> !`).

Rust:

```rust
use std::ops::Add;

#[no_mangle]
pub extern fn hello() {
    println!("Hello!");
}

fn add<O, R, L: Add<R, Output=O>>(l: L, r: R) -> O {
    l + r
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn MyAdd_add_u8(l: u8, r: u8) -> u8 {
    add(l, r)
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn MyAdd_add_u16(l: u16, r: u16) -> u16 {
    add(l, r)
}
```

Header:

```C
// Some boilerplate omitted.
void hello();

uint8_t MyAdd_add_u8(uint8_t l, uint8_t r);

uint16_t MyAdd_add_u16(uint16_t l, uint16_t r);
// Some more boilerplate omitted.
```

### Paths

You must not put types defined in other modules in an exported
type signature without hiding it behind an opaque struct.
This is because the C compiler must know the layout of the type
and moz-cheddar can not yet search other modules.

The very important exception to this rule are the C ABI types defined in
the `libc` crate and `std::os::raw`. Types from these two modules _must_
be fully qualified (e.g. `libc::c_void` or `std::os::raw::c_longlong`)
so that they can be converted properly. Importing them with a `use`
statement will not work.

[the cargo docs]: http://doc.crates.io/build-script.html
[repo]: https://github.com/mozilla/moz-cheddar

## Contributing

Contributions to moz-cheddar are more than welcome.

### Bugs

If you find a bug or have a feature request please open an issue.

If you find the source code unclear in any way then I consider that a bug.
I try to make my source code as clear as possible, so any help in that
regard is appreciated.

Pull requests are also welcome, of course.

#### Tests

The tests require you to have a version (> `v2.7.2`) of [CppHeaderParser]
installed for the version of Python which is installed as `python`
(usually Python 2). Furthermore, due to the fact that the tests are a
massive pile of wanky hacks, you must be in the same directory as
moz-cheddar's `Cargo.toml` to successfully run them.

If you don't have this, try
```bash
pip install CppHeaderParser
```

[rusty-binder]: https://gitlab.com/rusty-binder/rusty-binder
[these docs]: http://manishearth.github.io/rust-internals-docs/syntax/ast/index.html
[CppHeaderParser]: https://bitbucket.org/senex/cppheaderparser
