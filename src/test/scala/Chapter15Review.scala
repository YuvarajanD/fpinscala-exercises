import org.specs2.mutable.Specification

class Chapter15Review extends Specification  { val txt =
  """
    Stream processing

Nitpicking:

     when presenting flatMap/unit on Process:

    |Incidentally, Process forms a Monad. The unit function just emits a single
    |value, then halts:
    |def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    |  case Halt() => Halt()
    |  case Emit(h, t) =>
    |   if (h.isEmpty) t flatMap f
    |   else f(h.head) ++ emitAll(h.tail, t).flatMap(f)
    |  case Await(recv,fb) =>
    |    Await(recv andThen (_ flatMap f), fb flatMap f)
    | }
    |
    |def unit[O](o: => O): Process[I,O] = emit(o)
    |

    flatMap is defined on Process and unit in the companion object. In the case of Unit above it is not clear where the I type
    parameter comes from.


  The repeat method loops forever
    |def repeat: Process[I,O] = {
    |def go(p: Process[I,O]): Process[I,O] = p match {
    |case Halt() => go(this)
    |case Await(recv,fb) => Await(recv andThen go, fb)
    |case Emit(h, t) => Emit(h, go(t))
    |}
    |go(this)
    |}

  """

}
