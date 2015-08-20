Haskell-style Dialect of Rust [Working Draft]
================================================================================
This dialect is just an alternate syntax for Rust in order to bring the
terseness of Haskell to the Rust land.
The purpose is to reduce the number of characters to be typed as well as enhance
the legibility of the code.

Some Symbols
--------------------------------------------------------------------------------
*   `^` extern
*   `@` crate
*   `#` mut
*   `+` public
*   `-` private
*   `#` protected
*   `->` as
*   `=>` where

Modules
--------------------------------------------------------------------------------
An example of a module in rust's primary dialect:

``` {.rust .rust-example-rendered}
mod math {
    type Complex = (f64, f64);
    fn sin(f: f64) -> f64 {
        /* ... */
    }
    fn cos(f: f64) -> f64 {
        /* ... */
    }
    fn tan(f: f64) -> f64 {
        /* ... */
    }
}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
mod math =>

    type Complex = (f64, f64)
    
    sin : f64 -> f64
    sin f = {- ... -}
    
    cos : f64 -> f64
    cos f = {- ... -}
    
    tan : f64 -> f64
    tan f = {- ... -}
```

A module without a body is loaded from an external file, by default with
the same name as the module, plus the `.rs` extension.
When a nested submodule is loaded from an external file, it is loaded from a
subdirectory path that mirrors the module hierarchy.

``` {.rust .rust-example-rendered}
// Load the `vec` module from `vec.rs`
mod vec;

mod thread {
    // Load the `local_data` module from `thread/local_data.rs`
    // or `thread/local_data/mod.rs`.
    mod local_data;
}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
mod vec

mod thread =>
    -- indentation is significant here
    -- Load the `local_data` module from `thread/local_data.rs`
    -- or `thread/local_data/mod.rs`.
    mod local_data
```

The directories and files used for loading external file modules can be
influenced with the `path` attribute.

``` {.rust .rust-example-rendered}
#[path = "thread_files"]
mod thread {
    // Load the `local_data` module from `thread_files/tls.rs`
    #[path = "tls.rs"]
    mod local_data;
}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
#path=thread_files
mod thread =>
    -- Load the `local_data` module from `thread_files/tls.rs`
    #path=tls.rs
    mod local_data
```

Crate
--------------------------------------------------------------------------------
Three examples of `extern crate` declarations:

``` {.rust .rust-example-rendered}
extern crate pcre;

extern crate std; // equivalent to: extern crate std as std;

extern crate std as ruststd; // linking to 'std' under another name
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
@^pcre

@^std

@^std -> ruststd
```

#### [6.1.2.2 Use declarations](#use-declarations) {#use-declarations .section-header}

Rebinding the target name as a new local name, using the syntax

`use p::q::r as x;`

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
use p.q.r -> x
```

Simultaneously binding a list of paths differing only in their final element,
using the glob-like brace syntax

`use a::b::{c,d,e,f};`

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
use a.b.{c,d,e,f}
```

Binding all paths matching a given prefix, using the asterisk wildcard syntax

`use a::b::*;`

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
use a.b.*
```

Simultaneously binding a list of paths differing only in their final element and
their immediate parent module, using the `self` keyword, such as

`use a::b::{self, c, d};`

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
use a.b.{self,c,d}
```

An example of `use` declarations:

``` {.rust .rust-example-rendered}
use std::option::Option::{Some, None};
use std::collections::hash_map::{self, HashMap};

fn foo<T>(_: T){}
fn bar(map1: HashMap<String, usize>, map2: hash_map::HashMap<String, usize>){}

fn main() {
    // Equivalent to 'foo(vec![std::option::Option::Some(1.0f64), std::option::Option::None]);'
    foo(vec![Some(1.0f64), None]);

    // Both `hash_map` and `HashMap` are in scope.
    let map1 = HashMap::new();
    let map2 = hash_map::HashMap::new();
    bar(map1, map2);
}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
use std.option.Option        (Some None)
use std.collections.hash_map (self HashMap)

foo : T -> ()
foo _ = ()

bar : HashMap String usize -> hash_map.HashMap String usize -> ()
bar map1 map2 = ()

main : ()
main = foo vec![Some 1.0f64, None]
       let map1 = HashMap.new
       let map2 = hash_map.HashMap.new
       bar map1 map2

```

An example of re-exporting.
In this example, the module `quux` re-exports two public names defined in `foo`.

``` {.rust .rust-example-rendered}
mod quux {
    pub use quux::foo::{bar, baz};

    pub mod foo {
        pub fn bar() { }
        pub fn baz() { }
    }
}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
mod quux =>
    use quux.foo.{bar,baz} +

    mod foo + =>
        bar : () +
        bar = ()
        
        baz : () +
        baz = ()
```

An example of what will and will not work for `use` items:

``` {.rust .rust-example-rendered}
use foo::baz::foobaz;    // good: foo is at the root of the crate

mod foo {

    mod example {
        pub mod iter {}
    }

    use foo::example::iter; // good: foo is at crate root
//  use example::iter;      // bad:  core is not at the crate root
    use self::baz::foobaz;  // good: self refers to module 'foo'
    use foo::bar::foobar;   // good: foo is at crate root

    pub mod bar {
        pub fn foobar() { }
    }

    pub mod baz {
        use super::bar::foobar; // good: super refers to module 'foo'
        pub fn foobaz() { }
    }
}

fn main() {}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
use foo.baz.foobaz

mod foo =>

    mod example =>
        mod iter + =>
        
    use foo.example.iter    --good: foo is at crate root
    -- use example.iter     --bad:  core is not at the crate root
    use self.baz.foobaz     --good: self refers to module 'foo'
    use foo.bar.foobar      --good: foo is at crate root
    
    mod bar + =>
        foobar : () +
        foobar = ()
    
    mod baz + =>
        use super.bar.foobar    --good: super refers to module 'foo'
        foobaz : () +
        foobaz = ()
        
main : ()
main = ()
```

Functions
--------------------------------------------------------------------------------
An example of a function:

``` {.rust .rust-example-rendered}
fn add(x: i32, y: i32) -> i32 {
    return x + y;
}
```

In haskell-style dialect of rust

```{.haskell}
add : i32 -> i32 -> i32
add x y = x + y
```

As with `let` bindings, function arguments are irrefutable patterns, so
any pattern that is valid in a let binding is also valid as an argument.

``` {.rust .rust-example-rendered}
fn first((value, _): (i32, i32)) -> i32 { value }
```

In haskell-style dialect of rust

```{.haskell}
first : (i32, i32) -> i32
first (value, _) = value
```


### Generic functions

An example of a generic function:

``` {.rust .rust-example-rendered}
// foo is generic over A and B

fn foo<A, B>(x: A, y: B) {
```

In haskell-style dialect of rust

```{.haskell}
foo : A -> B -> ()
foo x y =
```

Inside the function signature and body, the name of the type parameter
can be used as a type name. [Trait](#traits) bounds can be specified for
type parameters to allow methods with that trait to be called on values
of that type. This is specified using the `where` syntax:

``` {.rust .rust-example-rendered}
fn foo<T>(x: T) where T: Debug {
```

In haskell-style dialect of rust

```{.haskell}
foo : Debug T => T -> ()
foo x =
```

When a generic function is referenced, its type is instantiated based on
the context of the reference. For example, calling the `foo` function
here:

``` {.rust .rust-example-rendered}
use std::fmt::Debug;

fn foo<T>(x: &[T]) where T: Debug {
    // details elided
}

foo(&[1, 2]);
```

will instantiate type parameter `T` with `i32`.
In haskell-style dialect of rust

```{.haskell}
foo : Debug T => &[T] -> ()
foo x = --details

foo &[1, 2]
```

The type parameters can also be explicitly supplied in a trailing
[path](#paths) component after the function name. This might be
necessary if there is not sufficient context to determine the type
parameters. For example, `mem::size_of::<u32>() == 4`.


### Diverging functions

A special kind of function can be declared with a `!` character where
the output type would normally be. For example:

``` {.rust .rust-example-rendered}
fn my_err(s: &str) -> ! {
    println!("{}", s);
    panic!();
}
```

In haskell-style dialect of rust

```{.haskell}
my_err : &str -> !
my_err s = println! "{}" s
           panic!
```

We call such functions "diverging" because they never return a value to
the caller.
Every control path in a diverging function must end with a
`panic!()` or a call to another diverging function on every control
path.
The `!` annotation does *not* denote a type.

It might be necessary to declare a diverging function because as
mentioned previously, the typechecker checks that every control path in
a function ends with a [`return`](#return-expressions) or diverging
expression. So, if `my_err` were declared without the `!` annotation,
the following code would not typecheck:

``` {.rust .rust-example-rendered}

fn f(i: i32) -> i32 {
   if i == 42 {
     return 42;
   }
   else {
     my_err("Bad number!");
   }
}
```

In haskell-style dialect of rust

```{.haskell}
f : i32 -> i32
f i
  | i == 42 = 42
  else      = my_err "Bad number!"
```

This will not compile without the `!` annotation on `my_err`, since the
`else` branch of the conditional in `f` does not return an `i32`, as
required by the signature of `f`. Adding the `!` annotation to `my_err`
informs the typechecker that, should control ever enter `my_err`, no
further type judgments about `f` need to hold, since control will never
resume in any context that relies on those judgments. Thus the return
type on `f` only needs to reflect the `if` branch of the conditional.


### Extern functions

Extern functions are part of Rust's foreign function interface,
providing the opposite functionality to [external
blocks](#external-blocks). Whereas external blocks allow Rust code to
call foreign code, extern functions with bodies defined in Rust code
*can be called by foreign code*. They are defined in the same way as any
other Rust function, except that they have the `extern` modifier.

``` {.rust .rust-example-rendered}
// Declares an extern fn, the ABI defaults to "C"
extern fn new_i32() -> i32 { 0 }

// Declares an extern fn with "stdcall" ABI
extern "stdcall" fn new_i32_stdcall() -> i32 { 0 }
```

In haskell-style dialect of rust

```{.haskell}
--Declares an extern fn, the ABI defaults to "C"
new_i32 : i32 ^
new_i32 = 0

--Declares an extern fn with "stdcall" ABI
new_i32_stdcall : i32 ^stdcall
new_i32_stdcall = 0
```

Unlike normal functions, extern fns have type `extern "ABI" fn()`. This
is the same type as the functions declared in an extern block.

``` {.rust .rust-example-rendered}
let fptr: extern "C" fn() -> i32 = new_i32;
```

In haskell-style dialect of rust

```{.haskell}

```

Extern functions may be called directly from Rust code as Rust uses
large, contiguous stack segments like C.


Type aliases
--------------------------------------------------------------------------------
The following defines the type `Point` as a synonym for the type `(u8, u8)`,
the type of pairs of unsigned 8 bit integers:

``` {.rust .rust-example-rendered}
type Point = (u8, u8);
let p: Point = (41, 68);
```

In haskell-style dialect of rust

```{.haskell}
type Point = (u8, u8);
let p = Point (41, 68);
```


Structures
--------------------------------------------------------------------------------
A *structure* is a nominal structure type defined with the keyword `struct`.
An example of a `struct` item and its use:

``` {.rust .rust-example-rendered}
struct Point {x: i32, y: i32}
let p = Point {x: 10, y: 11};
let px: i32 = p.x;
```

In haskell-style dialect of rust

```{.haskell}
--data Point = Point i32 i32
data Point = Point { x:i32
                   , y:i32
                   }
let p = Point {x=10, y=11};
```

A *tuple structure* is a nominal [tuple type](#tuple-types), also
defined with the keyword `struct`. For example:

``` {.rust .rust-example-rendered}
struct Point(i32, i32);
let p = Point(10, 11);
let px: i32 = match p { Point(x, _) => x };
```

In haskell-style dialect of rust

```{.haskell}
data Point = Point (i32, i32)
let p = Point (10, 11)
let px = match p {
            Point (x,_) => x
         }
```

A *unit-like struct* is a structure without any fields, defined by
leaving off the list of fields entirely. Such types will have a single
value. For example:

``` {.rust .rust-example-rendered}
struct Cookie;
let c = [Cookie, Cookie, Cookie, Cookie];
```

In haskell-style dialect of rust

```{.haskell}
data Cookie
let c = [Cookie, Cookie, Cookie, Cookie]
```

The precise memory layout of a structure is not specified. One can
specify a particular layout using the [`repr` attribute](#ffi-attributes).


Enumerations
--------------------------------------------------------------------------------
An *enumeration* is a simultaneous definition of a nominal [enumerated
type](#enumerated-types) as well as a set of *constructors*, that can be
used to create or pattern-match values of the corresponding enumerated
type.

Enumerations are declared with the keyword `enum`.

An example of an `enum` item and its use:

``` {.rust .rust-example-rendered}
enum Animal {
  Dog,
  Cat
}

let mut a: Animal = Animal::Dog;
a = Animal::Cat;
```

In haskell-style dialect of rust

```{.haskell}
data Animal = Dog | Cat
let *a = Dog
a = Cat
```

Enumeration constructors can have either named or unnamed fields:

``` {.rust .rust-example-rendered}
enum Animal {
    Dog (String, f64),
    Cat { name: String, weight: f64 }
}

let mut a: Animal = Animal::Dog("Cocoa".to_string(), 37.2);
a = Animal::Cat { name: "Spotty".to_string(), weight: 2.7 };
```

In haskell-style dialect of rust

```{.haskell}
data Animal = Dog String f64 | Cat { name:String, weight:f64 }
let *a = Dog "Cocoa" 37.2
a = Cat {name="Spotty", weight=2.7}
```

In this example, `Cat` is a *struct-like enum variant*, whereas `Dog` is
simply called an enum variant.

Enums have a discriminant. You can assign them explicitly:

``` {.rust .rust-example-rendered}
enum Foo {
    Bar = 123,
}
```

In haskell-style dialect of rust

```{.haskell}
data Foo = Bar<123>
```

If a discriminant isn't assigned, they start at zero, and add one for
each variant, in order.

You can cast an enum to get this value:

``` {.rust .rust-example-rendered}
let x = Foo::Bar as u32; // x is now 123u32
```

In haskell-style dialect of rust

```{.haskell}
let x = Bar -> u32
```

This only works as long as none of the variants have data attached. If
it were `Bar(i32)`, this is disallowed.


Constant items
--------------------------------------------------------------------------------
A *constant item* is a named *constant value* which is not associated
with a specific memory location in the program. Constants are
essentially inlined wherever they are used, meaning that they are copied
directly into the relevant context when used. References to the same
constant are not necessarily guaranteed to refer to the same memory
address.

Constant values must not have destructors, and otherwise permit most
forms of data. Constants may refer to the address of other constants, in
which case the address will have the `static` lifetime. The compiler is,
however, still at liberty to translate the constant many times, so the
address referred to may not be stable.

Constants must be explicitly typed. The type may be `bool`, `char`, a
number, or a type derived from those primitive types. The derived types
are references with the `static` lifetime, fixed-size arrays, tuples,
enum variants, and structs.

``` {.rust .rust-example-rendered}
const BIT1: u32 = 1 << 0;
const BIT2: u32 = 1 << 1;

const BITS: [u32; 2] = [BIT1, BIT2];
const STRING: &'static str = "bitstring";

struct BitsNStrings<'a> {
    mybits: [u32; 2],
    mystring: &'a str
}

const BITS_N_STRINGS: BitsNStrings<'static> = BitsNStrings {
    mybits: BITS,
    mystring: STRING
};
```


Static items
--------------------------------------------------------------------------------
A *static item* is similar to a *constant*, except that it represents a
precise memory location in the program. A static is never "inlined" at
the usage site, and all references to it refer to the same memory
location. Static items have the `static` lifetime, which outlives all
other lifetimes in a Rust program. Static items may be placed in
read-only memory if they do not contain any interior mutability.

Statics may contain interior mutability through the `UnsafeCell`
language item. All access to a static is safe, but there are a number of
restrictions on statics:

-   Statics may not contain any destructors.
-   The types of static values must ascribe to `Sync` to allow
    threadsafe access.
-   Statics may not refer to other statics by value, only by reference.
-   Constants cannot refer to statics.

Constants should in general be preferred over statics, unless large
amounts of data are being stored, or single-address and mutability
properties are required.


### Mutable statics

If a static item is declared with the `mut` keyword, then it is allowed
to be modified by the program. One of Rust's goals is to make
concurrency bugs hard to run into, and this is obviously a very large
source of race conditions or other bugs. For this reason, an `unsafe`
block is required when either reading or writing a mutable static
variable. Care should be taken to ensure that modifications to a mutable
static are safe with respect to other threads running in the same
process.

Mutable statics are still very useful, however. They can be used with C
libraries and can also be bound from C libraries (in an `extern` block).

``` {.rust .rust-example-rendered}

static mut LEVELS: u32 = 0;

// This violates the idea of no shared state, and this doesn't internally
// protect against races, so this function is `unsafe`
unsafe fn bump_levels_unsafe1() -> u32 {
    let ret = LEVELS;
    LEVELS += 1;
    return ret;
}

// Assuming that we have an atomic_add function which returns the old value,
// this function is "safe" but the meaning of the return value may not be what
// callers expect, so it's still marked as `unsafe`
unsafe fn bump_levels_unsafe2() -> u32 {
    return atomic_add(&mut LEVELS, 1);
}
```

Mutable statics have the same restrictions as normal statics, except
that the type of the value is not required to ascribe to `Sync`.


Traits
--------------------------------------------------------------------------------
A *trait* describes an abstract interface that types can implement. This
interface consists of associated items, which come in three varieties:

-   functions
-   constants
-   types

Associated functions whose first parameter is named `self` are called
methods and may be invoked using `.` notation (e.g., `x.foo()`).

All traits define an implicit type parameter `Self` that refers to "the
type that is implementing this interface". Traits may also contain
additional type parameters. These type parameters (including `Self`) may
be constrained by other traits and so forth as usual.

Trait bounds on `Self` are considered "supertraits". These are required
to be acyclic. Supertraits are somewhat different from other constraints
in that they affect what methods are available in the vtable when the
trait is used as a [trait object](#trait-objects).

Traits are implemented for specific types through separate
[implementations](#implementations).

Consider the following trait:

``` {.rust .rust-example-rendered}
trait Shape {
    fn draw(&self, Surface);
    fn bounding_box(&self) -> BoundingBox;
}
```

In haskell-style dialect of rust

```{.haskell}
trait Shape =>
    draw : &self -> Surface -> ()
    bounding_box : &self -> BoundingBox
```

This defines a trait with two methods. All values that have
[implementations](#implementations) of this trait in scope can have
their `draw` and `bounding_box` methods called, using
`value.bounding_box()` [syntax](#method-call-expressions).

Traits can include default implementations of methods, as in:

``` {.rust .rust-example-rendered}
trait Foo {
    fn bar(&self);
    fn baz(&self) { println!("We called baz."); }
}
```

In haskell-style dialect of rust

```{.haskell}
trait Foo =>
    bar : &self -> ()
    baz : &self -> ()
    baz = println! "We called baz."
```

Here the `baz` method has a default implementation, so types that
implement `Foo` need only implement `bar`. It is also possible for
implementing types to override a method that has a default
implementation.

Type parameters can be specified for a trait to make it generic. These
appear after the trait name, using the same syntax used in [generic
functions](#generic-functions).

``` {.rust .rust-example-rendered}
trait Seq<T> {
   fn len(&self) -> u32;
   fn elt_at(&self, n: u32) -> T;
   fn iter<F>(&self, F) where F: Fn(T);
}
```

In haskell-style dialect of rust

```{.haskell}
trait Seq T =>
    len : &self -> u32
    elt_at : &self -> u32 -> T
    iter : Fn(T) F => &self -> F -> ()
    baz = println! "We called baz."
```

It is also possible to define associated types for a trait. Consider the
following example of a `Container` trait. Notice how the type is
available for use in the method signatures:

``` {.rust .rust-example-rendered}
trait Container {
    type E;
    fn empty() -> Self;
    fn insert(&mut self, Self::E);
}
```

In haskell-style dialect of rust

```{.haskell}
trait Container a =>
    type E
    empty : Self
    insert : &*self -> Self::E -> ()
```

In order for a type to implement this trait, it must not only provide
implementations for every method, but it must specify the type `E`.
Here's an implementation of `Container` for the standard library type
`Vec`:

``` {.rust .rust-example-rendered}
impl<T> Container for Vec<T> {
    type E = T;
    fn empty() -> Vec<T> { Vec::new() }
    fn insert(&mut self, x: T) { self.push(x); }
}
```

In haskell-style dialect of rust

```{.haskell}
impl Container (Vec T) =>
    type E = T
    empty = Vec.new
    insert self x = self.push x
```

Generic functions may use traits as *bounds* on their type parameters.
This will have two effects:

-   Only types that have the trait may instantiate the parameter.
-   Within the generic function, the methods of the trait can be called
    on values that have the parameter's type.

For example:

``` {.rust .rust-example-rendered}
fn draw_twice<T: Shape>(surface: Surface, sh: T) {
    sh.draw(surface);
    sh.draw(surface);
}
```

In haskell-style dialect of rust

```{.haskell}
draw_twice : Shape T => Surface -> T -> ()
draw_twice surface sh = sh.draw surface
                        sh.draw surface
```

Traits also define an [trait object](#trait-objects) with the same name
as the trait. Values of this type are created by coercing from a pointer
of some specific type to a pointer of trait type. For example, `&T`
could be coerced to `&Shape` if `T: Shape` holds (and similarly for
`Box<T>`). This coercion can either be implicit or
[explicit](#type-cast-expressions). Here is an example of an explicit
coercion:

``` {.rust .rust-example-rendered}
trait Shape { }
impl Shape for i32 { }
let mycircle = 0i32;
let myshape: Box<Shape> = Box::new(mycircle) as Box<Shape>;
```

In haskell-style dialect of rust

```{.haskell}
trait Shape a =>
impl Shape i32 =>
let mycircle = 0i32
let myshape = Box.new mycircle -> Box Shape
```

The resulting value is a box containing the value that was cast, along
with information that identifies the methods of the implementation that
was used. Values with a trait type can have [methods
called](#method-call-expressions) on them, for any method in the trait,
and can be used to instantiate type parameters that are bounded by the
trait.

Trait methods may be static, which means that they lack a `self`
argument. This means that they can only be called with function call
syntax (`f(x)`) and not method call syntax (`obj.f()`). The way to refer
to the name of a static method is to qualify it with the trait name,
treating the trait name like a module. For example:

``` {.rust .rust-example-rendered}
trait Num {
    fn from_i32(n: i32) -> Self;
}
impl Num for f64 {
    fn from_i32(n: i32) -> f64 { n as f64 }
}
let x: f64 = Num::from_i32(42);
```

In haskell-style dialect of rust

```{.haskell}
trait Num a where
    from_i32 : i32 -> a
impl Num f64 =>
    from_i32 n = n -> f64
let x:f64 = Num.from_i32 42
    
```

Traits may inherit from other traits. For example, in

``` {.rust .rust-example-rendered}
trait Shape { fn area(&self) -> f64; }
trait Circle : Shape { fn radius(&self) -> f64; }
```

In haskell-style dialect of rust

```{.haskell}
trait Shape where
    area : &self -> f64
trait Shape => Circle where
    radius : &self -> f64
```

the syntax `Circle : Shape` means that types that implement `Circle`
must also have an implementation for `Shape`. Multiple supertraits are
separated by `+`, `trait Circle : Shape + PartialEq { }`. In an
implementation of `Circle` for a given type `T`, methods can refer to
`Shape` methods, since the typechecker checks that any type with an
implementation of `Circle` also has an implementation of `Shape`.

In type-parameterized functions, methods of the supertrait may be called
on values of subtrait-bound type parameters. Referring to the previous
example of `trait Circle : Shape`:

``` {.rust .rust-example-rendered}
fn radius_times_area<T: Circle>(c: T) -> f64 {
    // `c` is both a Circle and a Shape
    c.radius() * c.area()
}
```

In haskell-style dialect of rust

```{.haskell}
radius_times_area : Circle T => T -> f64
radius_times_area c = c.radius * c.area -- `c` is both a Circle and a Shape
```

Likewise, supertrait methods may also be called on trait objects.

``` {.rust .rust-example-rendered}
let mycircle = Box::new(mycircle) as Box<Circle>;
let nonsense = mycircle.radius() * mycircle.area();
```

In haskell-style dialect of rust

```{.haskell}
let mycricle = Box.new mycircle -> Box Circle
let nonsense = mycircle.radius * mycircle.area
```


Implementations
--------------------------------------------------------------------------------
An *implementation* is an item that implements a [trait](#traits) for a
specific type.

Implementations are defined with the keyword `impl`.

``` {.rust .rust-example-rendered}
struct Circle {
    radius: f64,
    center: Point,
}

impl Copy for Circle {}

impl Clone for Circle {
    fn clone(&self) -> Circle { *self }
}

impl Shape for Circle {
    fn draw(&self, s: Surface) { do_draw_circle(s, *self); }
    fn bounding_box(&self) -> BoundingBox {
        let r = self.radius;
        BoundingBox{x: self.center.x - r, y: self.center.y - r,
         width: 2.0 * r, height: 2.0 * r}
    }
}
```

In haskell-style dialect of rust

```{.haskell}
data Circle = Circle { radiuis:f64
                     , center:Point
                     }

impl Copy Circle where

impl Clone Circle where
    clone &self = *self
    
impl Shape Circle where
    draw &@ s = do_draw_circle s *@
    bounding_box &@ = let r = @.radius
                      BoundingBox {x=@.center.x, y=@.center.y, width=2.0*r, height=2.0*r}
```

It is possible to define an implementation without referring to a trait.
The methods in such an implementation can only be used as direct calls
on the values of the type that the implementation targets. In such an
implementation, the trait type and `for` after `impl` are omitted. Such
implementations are limited to nominal types (enums, structs), and the
implementation must appear in the same crate as the `self` type:

``` {.rust .rust-example-rendered}
struct Point {x: i32, y: i32}

impl Point {
    fn log(&self) {
        println!("Point is at ({}, {})", self.x, self.y);
    }
}

let my_point = Point {x: 10, y:11};
my_point.log();
```

When a trait *is* specified in an `impl`, all methods declared as part
of the trait must be implemented, with matching types and type parameter
counts.

An implementation can take type parameters, which can be different from
the type parameters taken by the trait it implements. Implementation
parameters are written after the `impl` keyword.

``` {.rust .rust-example-rendered}
impl<T> Seq<T> for Vec<T> {
   /* ... */
}
impl Seq<bool> for u32 {
   /* Treat the integer as a sequence of bits */
}
```

### [6.1.11 External blocks](#external-blocks) {#external-blocks .section-header}

External blocks form the basis for Rust's foreign function interface.
Declarations in an external block describe symbols in external, non-Rust
libraries.

Functions within external blocks are declared in the same way as other
Rust functions, with the exception that they may not have a body and are
instead terminated by a semicolon.

Functions within external blocks may be called by Rust code, just like
functions defined in Rust. The Rust compiler automatically translates
between the Rust ABI and the foreign ABI.

A number of [attributes](#attributes) control the behavior of external
blocks.

By default external blocks assume that the library they are calling uses
the standard C "cdecl" ABI. Other ABIs may be specified using an `abi`
string, as shown here:

``` {.rust .rust-example-rendered}
// Interface to the Windows API
extern "stdcall" { }
```

The `link` attribute allows the name of the library to be specified.
When specified the compiler will attempt to link against the native
library of the specified name.

``` {.rust .rust-example-rendered}
#[link(name = "crypto")]
extern { }
```

The type of a function declared in an extern block is
`extern "abi" fn(A1, ..., An) -> R`, where `A1...An` are the declared
types of its arguments and `R` is the declared return type.


Visibility and Privacy
--------------------------------------------------------------------------------

These two terms are often used interchangeably, and what they are
attempting to convey is the answer to the question "Can this item be
used at this location?"

Rust's name resolution operates on a global hierarchy of namespaces.
Each level in the hierarchy can be thought of as some item. The items
are one of those mentioned above, but also include external crates.
Declaring or defining a new module can be thought of as inserting a new
tree into the hierarchy at the location of the definition.

To control whether interfaces can be used across modules, Rust checks
each use of an item to see whether it should be allowed or not. This is
where privacy warnings are generated, or otherwise "you used a private
item of another module and weren't allowed to."

By default, everything in Rust is *private*, with one exception. Enum
variants in a `pub` enum are also public by default. When an item is
declared as `pub`, it can be thought of as being accessible to the
outside world. For example:

``` {.rust .rust-example-rendered}
// Declare a private struct
struct Foo;

// Declare a public struct with a private field
pub struct Bar {
    field: i32
}

// Declare a public enum with two public variants
pub enum State {
    PubliclyAccessibleState,
    PubliclyAccessibleState2,
}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
Foo-
-- or just Foo

Bar+ field:i32

State = PubliclyAccessibleState | PubliclyAccessibleState2
```

With the notion of an item being either public or private, Rust allows
item accesses in two cases:

1.  If an item is public, then it can be used externally through any of
    its public ancestors.
2.  If an item is private, it may be accessed by the current module and
    its descendants.

These two cases are surprisingly powerful for creating module
hierarchies exposing public APIs while hiding internal implementation
details. To help explain, here's a few use cases and what they would
entail:

-   A library developer needs to expose functionality to crates which
    link against their library. As a consequence of the first case, this
    means that anything which is usable externally must be `pub` from
    the root down to the destination item. Any private item in the chain
    will disallow external accesses.

-   A crate needs a global available "helper module" to itself, but it
    doesn't want to expose the helper module as a public API. To
    accomplish this, the root of the crate's hierarchy would have a
    private module which then internally has a "public api". Because the
    entire crate is a descendant of the root, then the entire local
    crate can access this private module through the second case.

-   When writing unit tests for a module, it's often a common idiom to
    have an immediate child of the module to-be-tested named `mod test`.
    This module could access any items of the parent module through the
    second case, meaning that internal implementation details could also
    be seamlessly tested from the child module.

In the second case, it mentions that a private item "can be accessed" by
the current module and its descendants, but the exact meaning of
accessing an item depends on what the item is. Accessing a module, for
example, would mean looking inside of it (to import more items). On the
other hand, accessing a function would mean that it is invoked.
Additionally, path expressions and import statements are considered to
access an item in the sense that the import/expression is only valid if
the destination is in the current visibility scope.

Here's an example of a program which exemplifies the three cases
outlined above:

``` {.rust .rust-example-rendered}
// This module is private, meaning that no external crate can access this
// module. Because it is private at the root of this current crate, however, any
// module in the crate may access any publicly visible item in this module.
mod crate_helper_module {

    // This function can be used by anything in the current crate
    pub fn crate_helper() {}

    // This function *cannot* be used by anything else in the crate. It is not
    // publicly visible outside of the `crate_helper_module`, so only this
    // current module and its descendants may access it.
    fn implementation_detail() {}
}

// This function is "public to the root" meaning that it's available to external
// crates linking against this one.
pub fn public_api() {}

// Similarly to 'public_api', this module is public so external crates may look
// inside of it.
pub mod submodule {
    use crate_helper_module;

    pub fn my_method() {
        // Any item in the local crate may invoke the helper module's public
        // interface through a combination of the two rules above.
        crate_helper_module::crate_helper();
    }

    // This function is hidden to any module which is not a descendant of
    // `submodule`
    fn my_implementation() {}

    #[cfg(test)]
    mod test {

        #[test]
        fn test_my_implementation() {
            // Because this module is a descendant of `submodule`, it's allowed
            // to access private items inside of `submodule` without a privacy
            // violation.
            super::my_implementation();
        }
    }
}
```

For a rust program to pass the privacy checking pass, all paths must be
valid accesses given the two rules above. This includes all use
statements, expressions, types, etc.

### [6.2.1 Re-exporting and Visibility](#re-exporting-and-visibility) {#re-exporting-and-visibility .section-header}

Rust allows publicly re-exporting items through a `pub use` directive.
Because this is a public directive, this allows the item to be used in
the current module through the rules above. It essentially allows public
access into the re-exported item. For example, this program is valid:

``` {.rust .rust-example-rendered}
pub use self::implementation::api;

mod implementation {
    pub mod api {
        pub fn f() {}
    }
}
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
use self.implementation.api +

mod implementation
    mod api +
        f : () +
        f = ()
```


This means that any external crate referencing `implementation::api::f`
would receive a privacy violation, while the path `api::f` would be
allowed.

When re-exporting a private item, it can be thought of as allowing the
"privacy chain" being short-circuited through the reexport instead of
passing through the namespace hierarchy as it normally would.

[6.3 Attributes](#attributes) {#attributes .section-header}
-----------------------------

Any item declaration may have an *attribute* applied to it. Attributes
in Rust are modeled on Attributes in ECMA-335, with the syntax coming
from ECMA-334 (C\#). An attribute is a general, free-form metadatum that
is interpreted according to name, convention, and language and compiler
version. Attributes may appear as any of:

-   A single identifier, the attribute name
-   An identifier followed by the equals sign '=' and a literal,
    providing a key/value pair
-   An identifier followed by a parenthesized list of sub-attribute
    arguments

Attributes with a bang ("!") after the hash ("\#") apply to the item
that the attribute is declared within. Attributes that do not have a
bang after the hash apply to the item that follows the attribute.

An example of attributes:

``` {.rust .rust-example-rendered}
// General metadata applied to the enclosing module or crate.
#![crate_type = "lib"]

// A function marked as a unit test
#[test]
fn test_foo() {
  /* ... */
}

// A conditionally-compiled module
#[cfg(target_os="linux")]
mod bar {
  /* ... */
}

// A lint attribute used to suppress a warning/error
#[allow(non_camel_case_types)]
type int8_t = i8;
```

> **Note:** At some point in the future, the compiler will distinguish
> between language-reserved and user-available attributes. Until then,
> there is effectively no difference between an attribute handled by a
> loadable syntax extension and the compiler.

### [6.3.1 Crate-only attributes](#crate-only-attributes) {#crate-only-attributes .section-header}

-   `crate_name` - specify the crate's crate name.
-   `crate_type` - see [linkage](#linkage).
-   `feature` - see [compiler features](#compiler-features).
-   `no_builtins` - disable optimizing certain code patterns to
    invocations of library functions that are assumed to exist
-   `no_main` - disable emitting the `main` symbol. Useful when some
    other object being linked to defines `main`.
-   `no_start` - disable linking to the `native` crate, which specifies
    the "start" language item.
-   `no_std` - disable linking to the `std` crate.
-   `plugin` — load a list of named crates as compiler plugins, e.g.
    `#![plugin(foo, bar)]`. Optional arguments for each plugin, i.e.
    `#![plugin(foo(... args ...))]`, are provided to the plugin's
    registrar function. The `plugin` feature gate is required to use
    this attribute.

### [6.3.2 Module-only attributes](#module-only-attributes) {#module-only-attributes .section-header}

-   `no_implicit_prelude` - disable injecting `use std::prelude::*` in
    this module.
-   `path` - specifies the file to load the module from.
    `#[path="foo.rs"] mod bar;` is equivalent to
    `mod bar { /* contents of foo.rs */ }`. The path is taken relative
    to the directory that the current module is in.

### [6.3.3 Function-only attributes](#function-only-attributes) {#function-only-attributes .section-header}

-   `main` - indicates that this function should be passed to the entry
    point, rather than the function in the crate root named `main`.
-   `plugin_registrar` - mark this function as the registration point
    for [compiler plugins](book/compiler-plugins.html), such as loadable
    syntax extensions.
-   `start` - indicates that this function should be used as the entry
    point, overriding the "start" language item. See the "start"
    [language item](#language-items) for more details.
-   `test` - indicates that this function is a test function, to only be
    compiled in case of `--test`.
-   `should_panic` - indicates that this test function should panic,
    inverting the success condition.
-   `cold` - The function is unlikely to be executed, so optimize it
    (and calls to it) differently.

### [6.3.4 Static-only attributes](#static-only-attributes) {#static-only-attributes .section-header}

-   `thread_local` - on a `static mut`, this signals that the value of
    this static may change depending on the current thread. The exact
    consequences of this are implementation-defined.

### [6.3.5 FFI attributes](#ffi-attributes) {#ffi-attributes .section-header}

On an `extern` block, the following attributes are interpreted:

-   `link_args` - specify arguments to the linker, rather than just the
    library name and type. This is feature gated and the exact behavior
    is implementation-defined (due to variety of linker
    invocation syntax).
-   `link` - indicate that a native library should be linked to for the
    declarations in this block to be linked correctly. `link` supports
    an optional `kind` key with three possible values: `dylib`,
    `static`, and `framework`. See [external blocks](#external-blocks)
    for more about external blocks. Two examples:
    `#[link(name = "readline")]` and
    `#[link(name = "CoreFoundation", kind = "framework")]`.

On declarations inside an `extern` block, the following attributes are
interpreted:

-   `link_name` - the name of the symbol that this function or static
    should be imported as.
-   `linkage` - on a static, this specifies the [linkage
    type](http://llvm.org/docs/LangRef.html#linkage-types).

On `enum`s:

-   `repr` - on C-like enums, this sets the underlying type used
    for representation. Takes one argument, which is the primitive type
    this enum should be represented for, or `C`, which specifies that it
    should be the default `enum` size of the C ABI for that platform.
    Note that enum representation in C is undefined, and this may be
    incorrect when the C code is compiled with certain flags.

On `struct`s:

-   `repr` - specifies the representation to use for this struct. Takes
    a list of options. The currently accepted ones are `C` and `packed`,
    which may be combined. `C` will use a C ABI compatible struct
    layout, and `packed` will remove any padding between fields (note
    that this is very fragile and may break platforms which require
    aligned access).

### [6.3.6 Macro-related attributes](#macro-related-attributes) {#macro-related-attributes .section-header}

-   `macro_use` on a `mod` — macros defined in this module will be
    visible in the module's parent, after this module has been included.

-   `macro_use` on an `extern crate` — load macros from this crate. An
    optional list of names `#[macro_use(foo, bar)]` restricts the import
    to just those macros named. The `extern crate` must appear at the
    crate root, not inside `mod`, which ensures proper function of the
    [`$crate` macro variable](book/macros.html#the-variable-$crate).

-   `macro_reexport` on an `extern crate` — re-export the named macros.

-   `macro_export` - export a macro for cross-crate usage.

-   `no_link` on an `extern crate` — even if we load this crate for
    macros, don't link it into the output.

See the [macros section of the
book](book/macros.html#scoping-and-macro-import/export) for more
information on macro scope.

### [6.3.7 Miscellaneous attributes](#miscellaneous-attributes) {#miscellaneous-attributes .section-header}

-   `export_name` - on statics and functions, this determines the name
    of the exported symbol.
-   `link_section` - on statics and functions, this specifies the
    section of the object file that this item's contents will be
    placed into.
-   `no_mangle` - on any item, do not apply the standard name mangling.
    Set the symbol for this item to its identifier.
-   `packed` - on structs or enums, eliminate any padding that would be
    used to align fields.
-   `simd` - on certain tuple structs, derive the arithmetic operators,
    which lower to the target's SIMD instructions, if any; the `simd`
    feature gate is necessary to use this attribute.
-   `unsafe_no_drop_flag` - on structs, remove the flag that prevents
    destructors from being run twice. Destructors might be run multiple
    times on the same object with this attribute. To use this, the
    `unsafe_no_drop_flag` feature gate must be enabled.
-   `doc` - Doc comments such as `/// foo` are equivalent to
    `#[doc = "foo"]`.
-   `rustc_on_unimplemented` - Write a custom note to be shown along
    with the error when the trait is found to be unimplemented on
    a type. You may use format arguments like `{T}`, `{A}` to correspond
    to the types at the point of use corresponding to the type
    parameters of the trait of the same name. `{Self}` will be replaced
    with the type that is supposed to implement the trait but doesn't.
    To use this, the `on_unimplemented` feature gate must be enabled.

### [6.3.8 Conditional compilation](#conditional-compilation) {#conditional-compilation .section-header}

Sometimes one wants to have different compiler outputs from the same
code, depending on build target, such as targeted operating system, or
to enable release builds.

There are two kinds of configuration options, one that is either defined
or not (`#[cfg(foo)]`), and the other that contains a string that can be
checked against (`#[cfg(bar = "baz")]`). Currently, only
compiler-defined configuration options can have the latter form.

``` {.rust .rust-example-rendered}
// The function is only included in the build when compiling for OSX
#[cfg(target_os = "macos")]
fn macos_only() {
  // ...
}

// This function is only included when either foo or bar is defined
#[cfg(any(foo, bar))]
fn needs_foo_or_bar() {
  // ...
}

// This function is only included when compiling for a unixish OS with a 32-bit
// architecture
#[cfg(all(unix, target_pointer_width = "32"))]
fn on_32bit_unix() {
  // ...
}

// This function is only included when foo is not defined
#[cfg(not(foo))]
fn needs_not_foo() {
  // ...
}
```

This illustrates some conditional compilation can be achieved using the
`#[cfg(...)]` attribute. `any`, `all` and `not` can be used to assemble
arbitrarily complex configurations through nesting.

The following configurations must be defined by the implementation:

-   `debug_assertions`. Enabled by default when compiling
    without optimizations. This can be used to enable extra debugging
    code in development but not in production. For example, it controls
    the behavior of the standard library's `debug_assert!` macro.
-   `target_arch = "..."`. Target CPU architecture, such as `"x86"`,
    `"x86_64"` `"mips"`, `"powerpc"`, `"arm"`, or `"aarch64"`.
-   `target_endian = "..."`. Endianness of the target CPU, either
    `"little"` or `"big"`.
-   `target_family = "..."`. Operating system family of the target, e.
    g.  `"unix"` or `"windows"`. The value of this configuration option
        is defined as a configuration itself, like `unix` or `windows`.
-   `target_os = "..."`. Operating system of the target, examples
    include `"windows"`, `"macos"`, `"ios"`, `"linux"`, `"android"`,
    `"freebsd"`, `"dragonfly"`, `"bitrig"` or `"openbsd"`.
-   `target_pointer_width = "..."`. Target pointer width in bits. This
    is set to `"32"` for targets with 32-bit pointers, and likewise set
    to `"64"` for 64-bit pointers.
-   `unix`. See `target_family`.
-   `windows`. See `target_family`.

You can also set another attribute based on a `cfg` variable with
`cfg_attr`:

``` {.rust .rust-example-rendered}
#[cfg_attr(a, b)]
```

Will be the same as `#[b]` if `a` is set by `cfg`, and nothing
otherwise.

### [6.3.9 Lint check attributes](#lint-check-attributes) {#lint-check-attributes .section-header}

A lint check names a potentially undesirable coding pattern, such as
unreachable code or omitted documentation, for the static entity to
which the attribute applies.

For any lint check `C`:

-   `allow(C)` overrides the check for `C` so that violations will go
    unreported,
-   `deny(C)` signals an error after encountering a violation of `C`,
-   `forbid(C)` is the same as `deny(C)`, but also forbids changing the
    lint level afterwards,
-   `warn(C)` warns about violations of `C` but continues compilation.

The lint checks supported by the compiler can be found via
`rustc -W help`, along with their default settings. [Compiler
plugins](book/compiler-plugins.html#lint-plugins) can provide additional
lint checks.

``` {.rust .rust-example-rendered}
mod m1 {
    // Missing documentation is ignored here
    #[allow(missing_docs)]
    pub fn undocumented_one() -> i32 { 1 }

    // Missing documentation signals a warning here
    #[warn(missing_docs)]
    pub fn undocumented_too() -> i32 { 2 }

    // Missing documentation signals an error here
    #[deny(missing_docs)]
    pub fn undocumented_end() -> i32 { 3 }
}
```

This example shows how one can use `allow` and `warn` to toggle a
particular check on and off:

``` {.rust .rust-example-rendered}
#[warn(missing_docs)]
mod m2{
    #[allow(missing_docs)]
    mod nested {
        // Missing documentation is ignored here
        pub fn undocumented_one() -> i32 { 1 }

        // Missing documentation signals a warning here,
        // despite the allow above.
        #[warn(missing_docs)]
        pub fn undocumented_two() -> i32 { 2 }
    }

    // Missing documentation signals a warning here
    pub fn undocumented_too() -> i32 { 3 }
}
```

This example shows how one can use `forbid` to disallow uses of `allow`
for that lint check:

``` {.rust .rust-example-rendered}
#[forbid(missing_docs)]
mod m3 {
    // Attempting to toggle warning signals an error here
    #[allow(missing_docs)]
    /// Returns 2.
    pub fn undocumented_too() -> i32 { 2 }
}
```

### [6.3.10 Language items](#language-items) {#language-items .section-header}

Some primitive Rust operations are defined in Rust code, rather than
being implemented directly in C or assembly language. The definitions of
these operations have to be easy for the compiler to find. The `lang`
attribute makes it possible to declare these operations. For example,
the `str` module in the Rust standard library defines the string
equality function:

``` {.rust .rust-example-rendered}
#[lang = "str_eq"]
pub fn eq_slice(a: &str, b: &str) -> bool {
    // details elided
}
```

The name `str_eq` has a special meaning to the Rust compiler, and the
presence of this definition means that it will use this definition when
generating calls to the string equality function.

The set of language items is currently considered unstable. A complete
list of the built-in language items will be added in the future.

### [6.3.11 Inline attributes](#inline-attributes) {#inline-attributes .section-header}

The inline attribute suggests that the compiler should place a copy of
the function or static in the caller, rather than generating code to
call the function or access the static where it is defined.

The compiler automatically inlines functions based on internal
heuristics. Incorrectly inlining functions can actually make the program
slower, so it should be used with care.

`#[inline]` and `#[inline(always)]` always cause the function to be
serialized into the crate metadata to allow cross-crate inlining.

There are three different types of inline attributes:

-   `#[inline]` hints the compiler to perform an inline expansion.
-   `#[inline(always)]` asks the compiler to always perform an
    inline expansion.
-   `#[inline(never)]` asks the compiler to never perform an
    inline expansion.

### [6.3.12 `derive`](#derive) {#derive .section-header}

The `derive` attribute allows certain traits to be automatically
implemented for data structures. For example, the following will create
an `impl` for the `PartialEq` and `Clone` traits for `Foo`, the type
parameter `T` will be given the `PartialEq` or `Clone` constraints for
the appropriate `impl`:

``` {.rust .rust-example-rendered}
#[derive(PartialEq, Clone)]
struct Foo<T> {
    a: i32,
    b: T
}
```

The generated `impl` for `PartialEq` is equivalent to

``` {.rust .rust-example-rendered}
impl<T: PartialEq> PartialEq for Foo<T> {
    fn eq(&self, other: &Foo<T>) -> bool {
        self.a == other.a && self.b == other.b
    }

    fn ne(&self, other: &Foo<T>) -> bool {
        self.a != other.a || self.b != other.b
    }
}
```

### [6.3.13 Compiler Features](#compiler-features) {#compiler-features .section-header}

Certain aspects of Rust may be implemented in the compiler, but they're
not necessarily ready for every-day use. These features are often of
"prototype quality" or "almost production ready", but may not be stable
enough to be considered a full-fledged language feature.

For this reason, Rust recognizes a special crate-level attribute of the
form:

``` {.rust .rust-example-rendered}
#![feature(feature1, feature2, feature3)]
```

The same example rewritten in haskell-style dialect of rust:

```{.haskell}
#!
```


This directive informs the compiler that the feature list: `feature1`,
`feature2`, and `feature3` should all be enabled. This is only
recognized at a crate-level, not at a module-level. Without this
directive, all features are considered off, and using the features will
result in a compiler error.

The currently implemented features of the reference compiler are:

-   `advanced_slice_patterns` - See the [match
    expressions](#match-expressions) section for discussion; the exact
    semantics of slice patterns are subject to change, so some types are
    still unstable.

-   `slice_patterns` - OK, actually, slice patterns are just scary and
    completely unstable.

-   `asm` - The `asm!` macro provides a means for inline assembly. This
    is often useful, but the exact syntax for this feature along with
    its semantics are likely to change, so this macro usage must be
    opted into.

-   `associated_consts` - Allows constants to be defined in `impl` and
    `trait` blocks, so that they can be associated with a type or trait
    in a similar manner to methods and associated types.

-   `box_patterns` - Allows `box` patterns, the exact semantics of which
    is subject to change.

-   `box_syntax` - Allows use of `box` expressions, the exact semantics
    of which is subject to change.

-   `concat_idents` - Allows use of the `concat_idents` macro, which is
    in many ways insufficient for concatenating identifiers, and may be
    removed entirely for something more wholesome.

-   `custom_attribute` - Allows the usage of attributes unknown to the
    compiler so that new attributes can be added in a backwards
    compatible manner (RFC 572).

-   `custom_derive` - Allows the use of `#[derive(Foo,Bar)]` as sugar
    for `#[derive_Foo] #[derive_Bar]`, which can be user-defined
    syntax extensions.

-   `intrinsics` - Allows use of the "rust-intrinsics" ABI. Compiler
    intrinsics are inherently unstable and no promise about them
    is made.

-   `lang_items` - Allows use of the `#[lang]` attribute. Like
    `intrinsics`, lang items are inherently unstable and no promise
    about them is made.

-   `link_args` - This attribute is used to specify custom flags to the
    linker, but usage is strongly discouraged. The compiler's usage of
    the system linker is not guaranteed to continue in the future, and
    if the system linker is not used then specifying custom flags
    doesn't have much meaning.

-   `link_llvm_intrinsics` – Allows linking to LLVM intrinsics via
    `#[link_name="llvm.*"]`.

-   `linkage` - Allows use of the `linkage` attribute, which is
    not portable.

-   `log_syntax` - Allows use of the `log_syntax` macro attribute, which
    is a nasty hack that will certainly be removed.

-   `main` - Allows use of the `#[main]` attribute, which changes the
    entry point into a Rust program. This capability is subject
    to change.

-   `macro_reexport` - Allows macros to be re-exported from one crate
    after being imported from another. This feature was originally
    designed with the sole use case of the Rust standard library in
    mind, and is subject to change.

-   `non_ascii_idents` - The compiler supports the use of non-ascii
    identifiers, but the implementation is a little rough around the
    edges, so this can be seen as an experimental feature for now until
    the specification of identifiers is fully fleshed out.

-   `no_std` - Allows the `#![no_std]` crate attribute, which disables
    the implicit `extern crate std`. This typically requires use of the
    unstable APIs behind the libstd "facade", such as libcore
    and libcollections. It may also cause problems when using syntax
    extensions, including `#[derive]`.

-   `on_unimplemented` - Allows the `#[rustc_on_unimplemented]`
    attribute, which allows trait definitions to add specialized notes
    to error messages when an implementation was expected but not found.

-   `optin_builtin_traits` - Allows the definition of default and
    negative trait implementations. Experimental.

-   `plugin` - Usage of [compiler plugins](book/compiler-plugins.html)
    for custom lints or syntax extensions. These depend on compiler
    internals and are subject to change.

-   `plugin_registrar` - Indicates that a crate provides [compiler
    plugins](book/compiler-plugins.html).

-   `quote` - Allows use of the `quote_*!` family of macros, which are
    implemented very poorly and will likely change significantly with a
    proper implementation.

-   `rustc_attrs` - Gates internal `#[rustc_*]` attributes which may be
    for internal use only or have meaning added to them in the future.

-   `rustc_diagnostic_macros`- A mysterious feature, used in the
    implementation of rustc, not meant for mortals.

-   `simd` - Allows use of the `#[simd]` attribute, which is overly
    simple and not the SIMD interface we want to expose in the
    long term.

-   `simd_ffi` - Allows use of SIMD vectors in signatures for
    foreign functions. The SIMD interface is subject to change.

-   `staged_api` - Allows usage of stability markers and
    `#![staged_api]` in a crate. Stability markers are also attributes:
    `#[stable]`, `#[unstable]`, and `#[deprecated]` are the
    three levels.

-   `start` - Allows use of the `#[start]` attribute, which changes the
    entry point into a Rust program. This capability, especially the
    signature for the annotated function, is subject to change.

-   `struct_inherit` - Allows using struct inheritance, which is barely
    implemented and will probably be removed. Don't use this.

-   `struct_variant` - Structural enum variants (those with
    named fields). It is currently unknown whether this style of enum
    variant is as fully supported as the tuple-forms, and it's not
    certain that this style of variant should remain in the language.
    For now this style of variant is hidden behind a feature flag.

-   `thread_local` - The usage of the `#[thread_local]` attribute is
    experimental and should be seen as unstable. This attribute is used
    to declare a `static` as being unique per-thread leveraging LLVM's
    implementation which works in concert with the kernel loader and
    dynamic linker. This is not necessarily available on all platforms,
    and usage of it is discouraged.

-   `trace_macros` - Allows use of the `trace_macros` macro, which is a
    nasty hack that will certainly be removed.

-   `unboxed_closures` - Rust's new closure design, which is currently a
    work in progress feature with many known bugs.

-   `unsafe_no_drop_flag` - Allows use of the `#[unsafe_no_drop_flag]`
    attribute, which removes hidden flag added to a type that implements
    the `Drop` trait. The design for the `Drop` flag is subject to
    change, and this feature may be removed in the future.

-   `unmarked_api` - Allows use of items within a `#![staged_api]` crate
    which have not been marked with a stability marker. Such items
    should not be allowed by the compiler to exist, so if you need this
    there probably is a compiler bug.

-   `visible_private_types` - Allows public APIs to expose otherwise
    private types, e.g. as the return type of a public function. This
    capability may be removed in the future.

-   `allow_internal_unstable` - Allows `macro_rules!` macros to be
    tagged with the `#[allow_internal_unstable]` attribute, designed to
    allow `std` macros to call `#[unstable]`/feature-gated functionality
    internally without imposing on callers (i.e. making them behave like
    function calls in terms of encapsulation).

If a feature is promoted to a language feature, then all existing
programs will start to receive compilation warnings about `#![feature]`
directives which enabled the new feature (because the directive is no
longer necessary). However, if a feature is decided to be removed from
the language, errors will be issued (if there isn't a parser error
first). The directive in this case is no longer necessary, and it's
likely that existing code will break if the feature isn't removed.

If an unknown feature is found in a directive, it results in a compiler
error. An unknown feature is one which has never been recognized by the
compiler.
