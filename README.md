This project allows for translation between `grep` style regex strings 
and the regular expression data type utilized by 
[15-150](cs.cmu.edu/~15150).

# Syntax #

Any single character, e.g., `a` will match that character. To match a
literal version of a meta-character escape it with backslash, e.g., `\*`
will match the character `*`. Adjacent characters are treated as the
concatenation of those characters.

Characters can be grouped for other commands with parentheses (`(`, `)`).

Any atom (either a single character or characters grouped by
parentheses) followed by a `*` will be matched 0 or more times.

Two atoms can be alternatively matched with `|`. For instance `a|b`
would match either `a` or `b`.

## Examples ##

- `a` will match the single character `a`
- `a|b` will match either the single character `a` or the single
character `b`
- `a*` will match a 0 or more times
- `abc` will match the string `abc`
- `(ab)*c` will match `ab` 0 or more times followed by `c`
- `a|b*` will match either: 
    - `a`; or,
    - `b` 0 or more times

# License #

[MIT](LICENSE)

# Authors #

Code for parsing regular expression in string format into the internal
datatype was written by Cooper Pierce and James Gallicchio.

Code which utilizes `Regex.t` values to match and accept regex was
written by 15-150 course staff, including Michael Erdmann and Frank
Pfenning
