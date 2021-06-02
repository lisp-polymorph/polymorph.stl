# Polymorph Standard Library

Inspired by [generic-cl](https://github.com/alex-gutev/generic-cl) and  [specialization-store](https://github.com/markcox80/specialization-store), this library provides support for overloading common simple functions (in many other languages usually referred to as "operators") based on types. Supports user defined specializations as well. It also tries to be as strictly typed as possible, resulting in compile-time errors/warnings in case types are mismatched. However, there are ways to relax the restrictions in places where it would make sense (such as different numeric types comparison).

It may look like the whole thing consists of too many systems that are too small, but that is intentional. The main idea is, even if some users disagree with the overall design, they can still use small parts. It is also easier to debug and maintain several small systems rather than a single big one.


# Implementations support
It currently works  anywhere, where [adhoc polymorphic functions](https://github.com/digikar99/adhoc-polymorphic-functions) work. Cltl2 support is the most important thing.


# Overview

## Systems
### Maths
Ad hoc polymorphic: 
- `+`,`-`, `*`, `/` are arithmetic operators 
- `=`, `/=` are equality operators 
- `>`,`>=`, `<`,`<=` are inequality operators 

Parametric polymorphic:
- `min` and `max` are based on `<` (currently any kind of parametric polymorphism is just a convention).


### Copy-cast
Ad hoc polymorphic:
- `cast` function, similar to `coerce`.
- `copy` currently two versions: `deep-copy` and `shallow-copy`

Warning, these will probably be refactored to have ad-hoc `copy` and parametric deep/shallow versions.

### Access
Accessors to different properties.

Ad hoc polymorphic:
- `at` function is an extended version of `elt`.
- `front/back` are the first and last element of the container whenever that makes sense. Are SETFable.
- `emptyp`,`size` and `capacity` deal with container sizes.

### Macros
While there are currently no "polymorphic" macros, these macros are useful for the style of development this system suggests.
- `zapf` is for updating values value in place using its previous value. IS a generalization of macros like `incf` or `decf`.
- `bind*` unites 3 things: `let*`, `multiple-value-bind` and builtin type declarations. Uses `default` for filling out the values if type was provided, otherwise defaults to `nil`.
- `define-struct` is a simplification of `defstruct` that fills in default values using `default` and given types. It has accessors for slots defined via defpolymorph and is able to do simple inheritance. Very much alpha version, the name may change.

### Utility
Utility functions, may or may not be exported at all.
- `default` returns a reasonable default object for a given type.
- `ind` is an index type.
 
### Data structures
WIP. Will have at least maps (RB tree based), deques (array based) and double linked lists.
