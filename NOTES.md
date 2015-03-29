nested-routes
=============

For response types and methods, I will need a control flow accumulator (monad)
that maintains the preorder of the response types / methods when adding them
ad-hoc, and also maintains uniqueness of them (through overriding).

The definition of bind (or the binary operation, come to think of it) would
have to preserve the preorder in the accumulated result.

> Monad with () as a param ~ monoid with control flow? o_0? 
