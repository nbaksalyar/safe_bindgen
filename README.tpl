**_This is a fork of rusty-cheddar until such time as
the [rusty-binder version](https://gitlab.com/rusty-binder/rusty-cheddar)
is available._**

# {{crate}}

[![Build Status](https://travis-ci.org/mozilla/moz-cheddar.svg?branch=master)](https://travis-ci.org/mozilla/moz-cheddar)
[![crates.io](http://meritbadge.herokuapp.com/moz-cheddar)](https://crates.io/crates/moz-cheddar)
![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)

{{readme}}

## Contributing

Contributions to moz-cheddar are more than welcome.

### Bugs

If you find a bug or have a feature request please open an issue.

If you find the source code unclear in any way then I consider that a bug.
I try to make my source code as clear as possible, so any help in that
regard is appreciated.

Pull requests are also welcome, of course.

### Tests

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
