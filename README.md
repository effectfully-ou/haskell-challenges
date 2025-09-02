# Haskell challenges

This repo contains some challenges to solve. Challenges are Haskell-specific (even [GHC](https://www.haskell.org/ghc)-specific, I guess) and most of them likely do not translate to other programming languages. Each challenge is a standalone [`stack`](https://docs.haskellstack.org/en/stable/README) project (contributing support for [`cabal`](https://www.haskell.org/cabal) would be highly appreciated).

This repo exists for fun of those who want to participate, not for assessing anybody's skills, so please do not use it for that purpose, especially during the hiring process.

Selection criteria for a challenge:

1. difficulty: from "somewhat non-trivial" to "rather hard". There are no plans so far on making challenges that are downright easy or stupid hard. If you can't solve a challenge, that's because it's designed to be hard to solve
2. there's at least one solution that does not require writing a lot of code. Ideally, under a couple of dozens of lines of code. The greater the `time_spent_thinking / time_spent_typing` ratio is, the better
3. there aren't too many corner cases to handle. Handling corner cases is no fun
4. there's something Haskell-specific about the challenge. For example, it relies on the specifics of the evaluation model or the type system or the ecosystem etc
5. the challenge can be formulated in an easy-to-understand way

Not everything from the above has to hold for a challenge to get posted, but the intention is to satisfy as many requirements as possible.

Every challenge comes with a test suite and there always exists a solution that makes the tests pass.

Every challenge gets posted on Reddit when it's out.

If you think there's a problem with a challenge (ambiguous wording, invalid assumption, insufficient testing, anything), please consider filing a report in [issues](https://github.com/effectfully/haskell-challenges/issues) or drop me an email at `<my_GitHub_handle>@gmail.com` or write a message on Reddit (publicly in the thread of the particular challenge or via DM). Any other kind of feedback (including negative) is highly appreciated as well.

Currently contributions of new challenges are not accepted as I'm not sure where this whole thing is going and how I want to evolve it.

You're encouraged to post a solution to a challenge. When you post a solution please do that via a link rather than inline code in order not to spoil the fun for others.
