use std::fmt::{self, Write};
use std::ops::Deref;

pub struct IndentedOutput<'a> {
    inner: &'a mut String,
    indent: bool,
    indent_level: usize,
    indent_width: usize,
}

impl<'a> IndentedOutput<'a> {
    pub fn new(inner: &'a mut String, width: usize) -> Self {
        IndentedOutput {
            inner: inner,
            indent: true,
            indent_level: 0,
            indent_width: width,
        }
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn unindent(&mut self) {
        self.indent_level -= 1;
    }

    fn write_indent(&mut self) -> Result<(), fmt::Error> {
        for _ in 0..(self.indent_level * self.indent_width) {
            self.inner.write_char(' ')?;
        }

        Ok(())
    }
}

impl<'a> Write for IndentedOutput<'a> {
    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        if s.is_empty() {
            return Ok(());
        }

        let mut lines = s.lines().peekable();
        let mut indent = self.indent;

        while let Some(line) = lines.next() {
            if !line.is_empty() && indent {
                self.write_indent()?;
            }

            self.inner.write_str(line)?;

            if lines.peek().is_some() {
                self.inner.write_char('\n')?;
            }

            indent = true;
        }

        if s.ends_with('\n') {
            self.inner.write_char('\n')?;
            self.indent = true;
        } else {
            self.indent = false;
        }

        Ok(())
    }
}

impl<'a> Deref for IndentedOutput<'a> {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke() {
        let mut output = String::new();
        let mut output = IndentedOutput::new(&mut output, 4);

        write!(output, "pub struct Foo {{\n").unwrap();
        output.indent();
        write!(output, "bar: Bar,\n").unwrap();
        write!(output, "baz").unwrap();
        write!(output, ": Baz,\n").unwrap();
        output.unindent();
        write!(output, "}}\n").unwrap();

        let expected = "pub struct Foo {\n    bar: Bar,\n    baz: Baz,\n}\n";
        assert_eq!(&*output, expected);
    }

    #[test]
    fn does_not_indent_empty_lines() {
        let mut output = String::new();
        let mut output = IndentedOutput::new(&mut output, 4);
        output.indent();

        write!(output, "foo\n\n\nbar").unwrap();

        let expected = "    foo\n\n\n    bar";
        assert_eq!(&*output, expected);
    }
}
