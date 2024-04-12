# Experiment into interior mutability in Oxc AST + visitors

## AST versions

1. Standard version - using `Box<'a, T>` for references between types.
2. Traversable version - identical, except references between types are `SharedBox<'a, T>`.

The difference between the two is that the traversable version features interior mutability
(via `GCell`). So the traversable AST can be mutated with just an immutable `&` reference.
It can also be traversed in any direction (up or down).

To avoid an expensive conversion process between the two AST versions, they are laid out in memory
exactly the same, and one can be transmuted to the other at zero cost.

## Links between nodes in traversable AST

`SharedBox<'a, T>` is an alias for `&'a GCell<T>`.
`GCell` is a cell type allowing interior mutability.
The traverable AST uses this type to allow traversing up or down the tree.

It maintains the no-aliasing invariant that you cannot obtain a `&mut` ref to an AST node while
also holding any other references to that node by widening the constraints to:

* You can hold as many immut `&` refs to AST nodes as you like simultaneously.
* To obtain a `&mut` ref to *any* node in the AST, you cannot simultaneously hold *any* refs
  to *any* node in the entire AST.

These constraints are enforced via the borrow-checker, and have zero runtime cost.

All AST nodes are doubly-linked - links down to their children, and a link up to their parent.
This allows limitless travel around the AST in any direction.

NB: Holding a `SharedBox<T>` is not the same as holding the node itself. You can hold as many
`SharedBox<T>`s as you like at any time, even while holding a `&mut` ref to an AST node.
You just cannot "open" a box to obtain a mutable ref to the node it contains with
`SharedBox<T>::borrow_mut` while any other box is "open".

## Cloning in traversable AST

`GCell` is not `Clone` or `Copy` but references to it (`SharedBox<T>` aka `&GCell<T>`) are.
Therefore `SharedBox<T>`s can be passed around easily, copied, or stored in context.

Cloning a `SharedBox<T>` only clones the *reference* and does not clone the *contents* of the box.
i.e. `let expr: SharedBox<Expression> = get_expr(); let expr2 = expr.clone();` does not create a 2nd
AST node, it only creates another *reference* to the *same* node.

`enum` AST node types (e.g. `Expression`) are `Copy` and `Clone`. As they only contain
a `SharedBox<T>` and the enum discriminant.

## Preventing illegal ASTs

Without specific handling to prevent it, it is possible to generate illegal ASTs with the same
node connected to the AST in 2 or more places.
This would be legal (but weird) in the the traversable AST, but is not legal in the standard AST,
which uses `Box` for "links" between AST nodes. `Box` owns its contents, so you cannot do
e.g. `let x = get_node(); let b1 = Box(x); let b2 = Box(x);`

We want to be able to transmute the traversable AST back to the standard AST, so we have to prevent
this in the traversable AST too. If we didn't, it would result in UB when using the standard AST
if it contains such illegal double-references.

The method to prevent this is currently runtime checks.
Every AST node has a `parent` field containing a `Parent<'a>` pointing to the parent.
An AST node which is connected to the AST has `parent` set to one of the contentful variants.
If a node is removed from the AST with a `take_*` method, the `parent` field is set to
`Parent::None`. `set_*` methods will only accept a node with `.parent == Parent::None`, and will
panic if passed an AST node which already has a full `parent`.
This prevents a node from being attached to the AST in more than 1 place.

To maintain these invariants, it is essential that access to `.parent` and other fields containing
nodes are not public outside this file. Alteration of these fields must only be allowed via
`take_*` and `set_*` methods, which do the necessary checks.

This implies that struct AST node types must *not* be `Clone`. If they were cloned, the `parent`
of the clone would be incorrect. Enum AST node types can be `Copy` and `Clone` as they don't have
a `parent` field. Each of their variants contains a `SharedBox<T>` ref to the specific node type,
and *those* contain the `parent`.

It would be better to enforce this invariant at compile time by wrapping nodes returned by `take_*`
in an `Orphan<T>` wrapper, and making `set_*` methods only accept `Orphan<T>`. TODO

## Cycles of nodes

The above does not prevent circular references between nodes, or even a node whose parent is itself.
However, this is fine from a safety perspective. Such a circular set of nodes by definition cannot
be connected to the tree which extends down from `Program`, so it's "floating in space" unconnected
to the AST.

Such a circular structure is illegal in the standard AST, and it would likely be UB to attempt to
traverse it in "standard land". But as all that's returned to "standard land" is the `Program`,
the illegal loop cannot be reached from there, and this unsound situation cannot arise.

In the traversable AST, if you're in the middle of traversing a node and it's turned into part of
a cycle, this will lead to an infinite loop - a traversal which keeps going around in circles.
Not good, but not UB. It would not be possible to prevent this statically, and even checking for
circularity at runtime would be fairly expensive, so we leave it as a situation that consumer of
the AST must ensure they avoid themselves when modifying the AST.
