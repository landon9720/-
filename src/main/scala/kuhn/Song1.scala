package kuhn

object Song1 extends Song {
  def part(a: Int, b: Int) = S(
    (0, a+0, B),
    (1, a+b, B-1),
    (2, a+12, B-2),
    (3, a+0, B-3)
  ) ** (3*B)
  import scala.util.Random.shuffle
  val song = (for { a ← shuffle(0 to 12); b ← shuffle(0 to 12) } yield part(a, b)) reduce (_>_)
}
