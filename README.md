# Cryptograms

A tool to solve cryptograms, specifically the ones in
*[Clever Cryptograms][cleverc]* by Louise B. Moll.

[cleverc]: http://www.cryptogramsbylouise.com/CleverC.htm

## What is solves

It solved cryptograms. By *cryptogram*, we mean a specific type of
substitution cypher.

A substitution cypher is when each glyph is swapped with another, and the
mapping used is the same for all glyphs across the entire encoded message.

To get needlessly formal about it, the mapping forms a bijection. That means
that no letter maps to itself, and every letter of the cypher text is paired
with exactly one letter in the plain text.

In our case, we only care about English alphabet letters. Any other glyphs are
passed over unchanged. We also assume a no identity rule, like Louise B. Moll
does.

## Requirements and Installation

Most of this is handled by [`stack`][stack].

[stack]: https://github.com/commercialhaskell/stack

## Usage

There isn't enough implemented to write out how to use this yet.

## How it solves it.

### Things that failed

I had a few failed attempts that seem worth mentioning.

One was a pure brute force. Try all possible maps and check to see if all
resulting words are in the dictionary. Unsurprisingly this took less time to
write than run.

I then tried to do a depth-first search of candidate words, where each cypher
text words was another layer in the search tree. I didn't prune well, so it
quickly got out of control.

### What it does now

For now this is a pretty high-level plan, since I haven't written enough code
to find all the problems with this approach.

* Build a dictionary.
* Build a candidate list for each word in the cyphertext
* Merge the candidates, removing ones which would make our mapping
  inconsistent
* Rebuild the plain text using the mapping.

## Resources

I'd like to thank Edwin Olson's work at <http://www.quipqiup.com/howwork.php>
and his paper on the topic for helping me think through how to make this work.

## License

It's MIT Licensed. See the included `LICENSE` file.
