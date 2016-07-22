import collection.mutable.ListBuffer
trait Signal {
  def len: Time
}
trait Sequence extends Signal
case class Notes(len: Time, notes: Seq[(Time, Note, Duration)] = Seq.empty) extends Sequence {
  override def toString = (
    for ((Time(time), Note(pitch), Duration(dur)) ← notes) yield s"$time $pitch $dur"
  ).mkString("", "\n", s"\nlen=$len")
}
case class Time(t: Long)
case class Note(pitch: Byte)
case class Duration(beats: Byte)
object Notes {
  def loop(x: Int)(f: ⇒ Notes): Notes = {
    var t = 0L
    val looped = new ListBuffer[(Time, Note, Duration)]
    for (y ← 0 until x) {
      val notes: Notes = f
      t += notes.len.t
      looped ++= (for ((Time(time), note, dur) ← notes.notes)
        yield (Time(notes.len.t * y + time), note, dur))
    }
    Notes(Time(t), looped)
  }
}
import Notes._
trait Audio extends Signal
trait NullSignal extends Signal
object NullSignal extends NullSignal {
  def len = Time(0)
}
trait Monad[-A <: Signal, +B <: Signal] {
  def apply(input: A): B
}
object Monad {
  implicit class MonadImplicits[A <: Signal, B <: Signal](monad: Monad[A, B]) {
    def map[B2 <: Signal](f: Monad[B, B2]) = new Monad[A, B2] {
      def apply(input: A): B2 = {
        val b: B = monad(input)
        val b2: B2 = f(b)
        b2
      }
    }
    def >>[B2 <: Signal](f: Monad[B, B2]): Monad[A, B2] = map(f)
  }
}
import Monad.MonadImplicits
class MasterOut extends Monad[Signal, NullSignal] {
  def apply(input: Signal) = {
    println(input)
    NullSignal
  }
}
case class Song(sequence: Sequence) extends Monad[NullSignal, Signal] {
  def apply(input: NullSignal) = sequence
}
object ƒ {
  def main(args: Array[String]) {
    val out = new MasterOut
    val song = Song(
      loop(4) {
        Notes(len = Time(2), notes = Seq(
          (Time(0), Note(0), Duration(1)),
          (Time(1), Note(1), Duration(1))
        ))
      }
    ) >> out
    song(NullSignal)
  }
}