import java.lang.Math._
import javax.sound.midi._
import collection.mutable.ListBuffer
import collection.mutable
// time, note, duration
case class Sequence(notes: Seq[(Int, Int, Int)], len: Int) {
  def midi: collection.Map[Int, List[MidiMessage]] = {
    val result = new mutable.HashMap[Int, ListBuffer[MidiMessage]]
    for ((t, note, d) ← notes) {
      val time = t * 12
      val end = time + d * 12
      if (!result.contains(time)) {
        result += time → new ListBuffer[MidiMessage]
      }
      result(time) += new ShortMessage(ShortMessage.NOTE_ON, 0, 60 + note, 90)
      if (!result.contains(end)) {
        result += end → new ListBuffer[MidiMessage]
      }
      result(end) += new ShortMessage(ShortMessage.NOTE_OFF, 0, 60 + note, 90)
    }
    result.mapValues(_.toList).toMap
  }
  override def toString = (
    for ((time, pitch, dur) ← notes) yield s"$time $pitch $dur"
  ).mkString("time pitch duration\n", "\n", s"\nlen=$len")
  def transposeTime(delta: Int): Sequence =
    new Sequence(for ((time, note, duration) ← notes) yield (delta + time, note, duration), delta + len)
}
object Sequence {
  def S(notes: (Int, Int, Int)*): Sequence = {
    val last = notes.maxBy(_._1)
    val len = last._1 + last._3 - 1
    new Sequence(notes, len)
  }
  val empty = Sequence(Seq.empty, 0)
  def rest(len: Int) = Sequence(Seq.empty, len)
}
trait Monad {
  def apply(input: Sequence): Sequence
}
object Monad {
  implicit class MonadImplicits(m0: Monad) {
    def ===(m1: Monad) = new Monad {
      override def apply(input: Sequence): Sequence = {
        val s0 = m0(input)
        val s1 = m1(input)
        Sequence(s0.notes ++ s1.notes, max(s0.len, s1.len))
      }
    }
    def >(m1: Monad) = new Monad {
      override def apply(input: Sequence): Sequence = {
        val s0 = m0(input)
        val s1 = m1(input)
        Sequence(s0.notes ++ s1.transposeTime(s0.len).notes, s0.len + s1.len)
      }
    }
    def >>(m1: Monad) = new Monad {
      def apply(input: Sequence): Sequence = {
        val s0 = m0(input)
        val s1 = m1(s0)
        s1
      }
    }
    def *(times: Int) = {
      var m = m0
      1 until times foreach { _ ⇒ m = m > m0 }
      m
    }
    def *:(times: Int) = *(times)
  }
}
import Monad._
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
object MasterOut extends Monad {
  def apply(input: Sequence) = {
    R.midi = input.midi.toMap
    Sequence.empty
  }
}
case class Part(sequence: Sequence) extends Monad {
  def apply(ignored: Sequence) = sequence
}
object ƒ {
  def p(notes: (Int, Int, Int)*): Part = Part(Sequence.S(notes:_*))
  def main(args: Array[String]) {
    val song =
      8 *: (
        p((1, 0, 1),
          (2, 4, 1),
          (3, 9, 1),
          (4, 4, 1)) ===
        p((1, -12, 4))
        )
    song >> MasterOut apply Sequence.empty
    MidiSystem.getTransmitter.setReceiver(R)
    readLine
    System.exit(0)
  }
};
