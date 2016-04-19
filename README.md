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

There isn't enough implemented to write out how to use this yet.

## Resources

I'd like to thank Edwin Olson's work at <http://www.quipqiup.com/howwork.php>
and his paper on the topic for helping me think through how to make this work.

## License

It's MIT Licensed. See the included `LICENSE` file.
