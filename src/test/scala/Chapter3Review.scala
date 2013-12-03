import org.specs2.mutable.Specification

class Chapter3Review extends Specification  { val txt =
  """

  * I'm not convinced about the sidebar about variance. If it's not important maybe it's better to remove it.
    A motivating example on why this is useful would be good, or at least a reference to an example/a discussion on the subject.
    Otherwise we don't really understand why the annotation is presented at all.

    Oh, actually 'Nil' is a good motivation for the annotation. Reformulating the paragraph so that Nil becomes a good example
    (and not just a remark introduced with "Notice that") of why the annotation is useful and why Nil is used in all the examples instead
    of List[Int], List[String] would be a bit better.

  * "We can convert a Seq[A], x, back into something that can be passed
    to a variadic function using the syntax x: _*"

    An example would be even better: List.apply(otherSequence:_*)

  * "This is an unfortunate artificial restriction of the Scala compiler; other functional languages like Haskell and OCaml
     provide  complete inference, meaning type annotations are never required."

    "Artificial restriction" is a bit strong here. My understanding is that Scala supports subtyping which make general type-inference
    undecidable.

  * I really liked the "fold" generalization exercise with Trees

  """
}
