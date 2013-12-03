import org.specs2.mutable._

class PrefaceReview extends Specification { val txt =
  """
  *How to read this book*

  "It will become clear that these libraries follow certain patterns, which highlights the need for new *cognitive* tools for abstracting"

   => I don't think that the word *cognitive* is appropriate here. This implies that we need to change our minds where we mostly need to use new abstraction.

  """
}
