# Cryptograms

A tool to solve cryptograms, specifically the ones in
*[Clever Cryptograms][cleverc]* by Louise B. Moll.

[cleverc]: http://www.cryptogramsbylouise.com/CleverC.htm

## What is solves

It solves cryptograms. By *cryptogram*, we mean a specific type of
substitution cypher.

A substitution cypher is when each glyph is swapped with another, and the
mapping used is the same for all glyphs across the entire encoded message.

In our case, we only care about uppercase English alphabet letters. Any other
glyphs are passed over unchanged. Unlike Louise B. Moll, we don't assume a no
identity rule.

## Requirements and Installation

Most of this is handled by [`stack`][stack].

[stack]: https://github.com/commercialhaskell/stack

This includes development stuff like dependency fetching and test running.

## Usage

``` txt
Usage: cryptogram [-e|--encrypt] [-k|--key ARG] MESSAGE [-d|--dictionary ARG]
```

To encrypt some text, pass the message in with the `--encrypt` flag set,
otherwise decryption is assumed.

A key can be specified. When encrypting without a key, one is randomly
generated. Decryption without a key will print out all solutions which decode
the valid-seeming words in the text.

Specifying a dictionary file assumes a dictionary with whitespace-delimited
words. You can get a decent dictionary for simple messages from
[here][google-dict]. By default it uses the dictionary at
`/usr/share/dict/words`, which is awful for this.

## A note on dictionaries

The dictionary you use matters a lot.

The default dictionary for me in OS X 10.11 has too many low probability
words, and is sorted lexicographically, so it is truly awful. Feeding it a
word or two from a puzzle is a decent way to get a hint though.

This program doesn't care about the order of the words in the dictionary, but
it preserves them when searching. That means it *will* try "cat" before "the"
using the default dictionary, if both are valid options.

If you use a version of the suggested dictionary from [first20hours][], it
will put more likely solutions first. If you're going to use it, you'll want
remove a bunch of the nonsense short words (it only takes a few minutes in a
REPL.)

[first20hours]: https://github.com/first20hours

## Resources

I'd like to thank Edwin Olson's work at <http://www.quipqiup.com/howwork.php>
and his paper on the topic for helping me think through how to make this work.

## License

It's [MIT][] Licensed. See the included `LICENSE` file.

[MIT]: https://opensource.org/licenses/MIT

[google-dict]: https://github.com/first20hours/google-10000-english
