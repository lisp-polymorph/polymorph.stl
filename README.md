# Polymorph Standard Library

Inspired by [generic-cl](https://github.com/alex-gutev/generic-cl) and  [polymorphic-functions](https://github.com/digikar99/polymorphic-functions), this library reimplements standard library functions (and adds some more) based on type dispatch. Supports user defined specializations as well. It also tries to be as strictly typed as possible, resulting in compile-time errors/warnings in case types are mismatched. However, there are ways to relax the restrictions in places where it would make sense (such as different numeric types comparison).

It may look like the whole thing consists of too many systems that are too small, but that is intentional. The main idea is, even if some users disagree with the overall design, they can still use small parts. It is also easier to debug and maintain several small systems rather than a single big one.


# Implementations support
It currently works anywhere [polymorphic-functions](https://github.com/digikar99/polymorphic-functions) work. Cltl2 support is the most important thing.


# Overview
For more detailed documentaion, look at the specific system README.

## Systems

### STL
This system's sole purpose is to load (and test) all other systems that I consider working. If you want to use full power of lisp-polymorph, use this system.


### Maths
Ad hoc polymorphic: 
- `+`,`-`, `*`, `/` are arithmetic functions
- `incf`, `decf`, `multf`, `divf` are updating macros corresponding to the functions above
- `=`, `/=` are equality functions
- `>`,`>=`, `<`,`<=` are inequality functions 

Parametric polymorphic:
- `min` and `max` are based on `<` and `>` (currently any kind of parametric polymorphism is just a convention).


### Copy-cast
Ad hoc polymorphic:
- `cast` function is similar to `coerce`. WIP.
- `copy` currently has two versions: `deep-copy` and `shallow-copy`

Warning, these will probably be refactored to have ad-hoc `copy` and parametric `deep`/`shallow` versions.


### Access
Accessors to different properties.

Ad hoc polymorphic:
- `at/at-safe` functions is an extended version of `elt/gethash`. Are SETFable.
- `front/front-safe/back/back-safe` are the first and last elements of the container whenever that makes sense. Are SETFable.
- `emptyp`,`size`, `capacity` deal with container sizes.


### Traversable
Biggest system so far. WIP. Implements:
- Standard traversing functions (map/reduce/find/etc). WIP.
- Iterator protocol and mirrors of the standard traversing functions that return iterators.
- (TODO) Extensions for iterate (since we already depend on it).
- (TODO) Maybe somehow base it on [series](https://cliki.net/SERIES)?


### Macros
While there are currently no "polymorphic" macros, these macros are useful for the style of development this system suggests.
- `zapf` is for updating values value in place using its previous value. It is a generalization of macros like `incf` or `decf`.
- `setf*` is for updating values in a manner similar to `setf` but in a type safe way. Can be annoying at times due to imperfections of type inference.
- `bind/bind*` unites 3 things: `let/let*`, `multiple-value-bind`, and builtin type declarations. Uses `default` for filling out the values if type was provided, otherwise defaults to `nil`.
- `def` is a simplification of `defstruct` that fills in default values using `default` and given types. It has accessors for slots defined via defpolymorph and is unable to do inheritance -- use composition. Very much alpha version, may change.


### Introspect-ctype
Utility functions (like `default`) and ctype processing. Uses [ctype](https://github.com/s-expressionists/ctype) as a backend.

 
### Data structures
WIP. So far has at least maps (RB tree based), deques (array based), priority-queues, double linked lists. I am planning to replicates standard structures as well, including vectors,  single-linked lists and hashmaps/sets. 


### Callable
WIP. Currently broken. Allows for customization of `funcall` and `apply`.


### Tests
Tests are distributed throughout the system. Run `(asdf:test-system 'polymorph.stl)`.


### Related Projects
[generic-cl](https://github.com/alex-gutev/generic-cl)
[polymorphic-functions](https://github.com/digikar99/polymorphic-functions)
[extensible-compound-types](https://github.com/digikar99/extensible-compound-types)
[cl-form-types](https://github.com/alex-gutev/cl-form-types)
[ctype](https://github.com/s-expressionists/ctype)


### Acknowledgements
[Alex Gutev](https://github.com/alex-gutev/) for inspiration and immense help with testing all the systems, as well as coauthoring `polymorph.traversable/callable`.
[Shubhamkar Ayare](https://github.com/digikar99) for providing the backend for this library. Without his efforts it would not be possible. His initial motivation for polymorphic-functions came from his library [dense-arrays](https://github.com/digikar99/dense-arrays), check it out.
