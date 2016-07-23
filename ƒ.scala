import javax.sound.midi._
import scala.collection.mutable.ListBuffer
trait Signal {
  def len: Int
}
trait Sequence extends Signal
// time, note, duration
case class Notes(notes: Seq[(Int, Int, Int)] = Seq.empty, len: Int = 0) extends Sequence {
  def midi: collection.Map[Int, List[MidiMessage]] = {
    val result = new collection.mutable.HashMap[Int, collection.mutable.ListBuffer[MidiMessage]]
    for ((t, note, d) ← notes) {
      val time = t * 12
      val end = time + d * 12
      if (!result.contains(time)) {
        result += time → new collection.mutable.ListBuffer[MidiMessage]
      }
      result(time) += new ShortMessage(ShortMessage.NOTE_ON, 0, 60 + note, 90)
      if (!result.contains(end)) {
        result += end → new collection.mutable.ListBuffer[MidiMessage]
      }
      result(end) += new ShortMessage(ShortMessage.NOTE_OFF, 0, 60 + note, 90)
    }
    result.mapValues(_.toList).toMap
  }
  def transport(delta: Int): Seq[(Int, Int, Int)] = for ((time, note, duration) ← notes) yield (time + delta, note, duration)
  override def toString = (
    for ((time, pitch, dur) ← notes) yield s"$time $pitch $dur"
  ).mkString("time pitch duration\n", "\n", s"\nlen=$len")
}
object Notes {
  def loop(x: Int)(f: ⇒ Notes): Notes = {
    var t = 0
    val looped = new ListBuffer[(Int, Int, Int)]
    for (y ← 0 until x) {
      val notes: Notes = f
      looped ++= notes.transport(t * y)
      t += notes.len
    }
    Notes(looped, t)
  }
  def rest(len: Int) = Notes(Seq.empty, len)
}
import Notes._
trait Audio extends Signal
trait NullSignal extends Signal
object NullSignal extends NullSignal {
  def len = 0
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
object Midi {
  val SongPositionPointer = 0xf2.toByte
  val TimingClock = 0xf8.toByte
  implicit class ByteImplicits(byte: Byte) {
    def hex = Option(byte).map("%02x" format _).mkString
  }
  implicit class ByteArrayImplicits(bytes: Array[Byte]) {
    def hex = bytes.map("0x%02x" format _).mkString(" ")
  }
  implicit class MidiMessageImplicits(message: MidiMessage) {
    def toDebugString = {
      s"type=${message.getClass.getName} status=${message.getStatus.toByte.hex} length=${message.getLength} message=${message.getMessage.hex}"
    }
  }
}
import Midi._
object R extends Receiver {
  var midi = collection.immutable.Map.empty[Int, List[MidiMessage]]
  var pos = 0
  override def send(message: MidiMessage, timeStamp: Long): Unit = {
    message.getMessage match {
      case Array(SongPositionPointer, lsb, msb) ⇒
        val v = lsb | (msb << 8)
        pos = v
      case Array(TimingClock) ⇒
        pos += 1
        for {
          messages ← midi.get(pos)
          message ← messages
        } {
          MidiSystem.getReceiver.send(message, -1)
        }
      case _ ⇒
        println(s"message=${message.toDebugString} timeStamp=$timeStamp")
    }
  }
  override def close(): Unit = {
    println(s"close")
  }
}
class MasterOut extends Monad[Signal, NullSignal] {
  def apply(input: Signal) = {
    val notes = input.asInstanceOf[Notes]
    R.midi = notes.midi.toMap
    NullSignal
  }
}
case class Song(sequence: Sequence) extends Monad[NullSignal, Signal] {
  def apply(input: NullSignal) = sequence
}
object ƒ {
  implicit class ImplicitNoteFactory(record0: (Int, Int)) {
    def +(record1: (Int, Int)): Notes = Notes(Seq((0, record0._1, record0._2), (record0._2, record0._1, record0._2)), record0._2 + record1._2)
  }
  implicit class ImplicitNotes(notes0: Notes) {
    def +(record: (Int, Int)): Notes = Notes(notes0.notes :+ (notes0.len, record._1, record._2), notes0.len + record._2)
    def +(notes1: Notes): Notes = Notes(notes0.notes ++ notes1.transport(notes0.len), notes0.len + notes1.len)
  }
  def main(args: Array[String]) {
    val out = new MasterOut
    val song = Song(
      rest(2) +
      loop(4) {
        loop(2) {
          (0, 1) +
          (0, 1) +
          (0, 2)
        } +
        loop(2) {
          (0, 2) +
          (0, 2)
        }
      }
    ) >> out
    song(NullSignal)
    MidiSystem.getTransmitter.setReceiver(R)
    readLine
    System.exit(0)
  }
}