# Stainless formalization of the bundle closure algorithm.

See: https://github.com/opencaesar/owl-tools/tree/master/owl-close-world

## Using stainless to improve the specification.

### [v1 commit](https://github.com/NicolasRouquette/stainless-bundle-closure/tree/v1)

This is the initial version converted from Java.
Stainless finds a few interesting things:

```scala
  def intersection(e: ClassExpression): ClassExpression = // VC 'non-negative measure' @ 18:7: VALID
    if (this == e)
      this
    else e match {
      case Universal =>
        e.intersection(this) // VC 'measure decreases' @ 23:23: INVALID
      case _: Intersection =>
        e.intersection(this) // VC 'measure decreases' @ 25:23: VALID
      case Empty =>
        e.intersection(this) // VC 'measure decreases' @ 27:23: INVALID
      case _ =>
        Intersection(Cons(this, Cons(e, Nil())))
    }
```

The problem here is that these methods need to be written in an inductive style
where, in some sense, the different paths in the function result in a measurable
decrease in terms of the "size" of the object that the function is applied to.

With a conventional object-oriented style, this measurable decrease does not hold
because in effect these methods are "constructors" for building composite objects
from simpler ones.

### [v2 commit](https://github.com/NicolasRouquette/stainless-bundle-closure/tree/v2)

This refactoring eliminates the stainless errors; however,
without additional annotations, stainless can't verify much about this code.
