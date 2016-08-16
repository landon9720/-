package kuhn

case class Scale(ranks: List[Int]) extends Monad {
  override def apply(input: Sequence): Sequence =
    Sequence(for (value ← input.values) yield {
      var rank = value.value
      var octave = value.octave
      while (rank < 0) {
        rank += ranks.size
        octave -= 1
      }
      while (rank >= ranks.size) {
        rank -= ranks.size
        octave += 1
      }
      value.copy(value = ranks(rank) + value.accidental, octave = octave)
    })
}