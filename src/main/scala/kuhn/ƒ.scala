package kuhn

import java.lang.Math._
import javax.sound.midi._
import scala.collection.{mutable, _}
import scala.collection.mutable.ListBuffer

object ƒ {
  val beats = 12
  type Ratio = (Int, Int)
  implicit class RichInt(i: Int) {
    def productWithRatio(ratio: Ratio): Int = {
      val (numerator, denominator) = ratio
      (i * numerator) / denominator
    }
  }
}
import ƒ._

case class Value(time: Int, duration: Int, value: Int, octave: Int, accidental: Int, attack: Int, attackFine: Int, release: Int, releaseFine: Int) {
  def copyWithTime(time: Int): Value = copy(time = time)
  def copyWithValue(value: Int): Value = copy(value = value)
}

case class Sequence(
  val events: Seq[Value],
  val duration: Int
) {
  override def toString = (
    for (e ← events) yield s"$e"
  ).mkString("events\n", "\n", s"\nduration=$duration")
  def transport(delta: Int) =
    copy(for (e ← events) yield e.copyWithTime(delta + e.time), delta + duration)
}

object Sequence {
  val empty = new Sequence(Seq.empty, 0)
}

trait Monad {
  def apply(input: Sequence): Sequence
}

object Monad {
  implicit def functionToMonad(f: Sequence ⇒ Sequence): Monad = new Monad {
    override def apply(input: Sequence): Sequence = f(input)
  }
  implicit class MonadImplicits(m0: Monad) {
    def ===(m1: Monad): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        val s1 = m1(input)
        s0.copy(s0.events ++ s1.events, max(s0.duration, s1.duration))
    }
    def >(m1: Monad): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        val s1 = m1(input)
        s0.copy(s0.events ++ s1.transport(s0.duration).events, s0.duration + s1.duration)
    }
    def >>(m1: Monad): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        val s1 = m1(s0)
        s1
    }
    def *(times: Int) = {
      var m = m0
      1 until times foreach { _ ⇒ m = m > m0 }
      m
    }
    def *:(times: Int) = *(times)
    def **(scale: Int): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        s0.copy(s0.events map { e ⇒ e.copyWithTime(e.time * scale) }, s0.duration * scale)
    }
    def **:(scale: Int) = **(scale)
    def */(scale: Int): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        s0.copy(s0.events map { e ⇒ e.copyWithTime(e.time / scale) }, s0.duration / scale)
    }
    def */:(scale: Int) = **(scale)
    def normalize(to: Int): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        if (s0.duration < to) {
          (m0 ** (to / s0.duration))(input)
        } else if (s0.duration > to) {
          (m0 */ (s0.duration / to))(input)
        } else {
          s0
        }
    }
    def N = normalize(to = 1*beats)
  }
}
import kuhn.Monad._

object Midi {
  val SongPositionPointer = 0xf2 toByte
  val TimingClock = 0xf8 toByte
  val NoteOff = 0x80 toByte
  val NoteOff16 = NoteOff + (0xf toByte)
  val NoteOn = 0x90 toByte
  val NoteOn16 = NoteOn + (0xf toByte)
  val Start = 0xfa toByte
  val Continue = 0xfb toByte
  val Stop = 0xfc toByte
  val ControlChange = 0xb0 toByte
  val ControlChange16 = ControlChange + (0xf toByte)
  val PitchBend = 0xe0
  val PitchBend16 = PitchBend + (0xf toByte)
  implicit class ByteImplicits(byte: Byte) {
    def hex = Option(byte).map("%02x" format _).mkString
  }
  implicit class ByteArrayImplicits(bytes: Array[Byte]) {
    def hex = bytes.map("0x%02x" format _).mkString(" ")
  }
  implicit class MidiMessageImplicits(message: MidiMessage) {
    def toDebugString = {
      s"message=${message.getMessage.hex} length=${message.getLength}"
    }
  }
}
import kuhn.Midi._

class MidiReceiver(positionUpdate: Int ⇒ Unit) extends Receiver {
  var midi = immutable.Map.empty[Int, List[MidiMessage]]
  var pos = 0
  private val receiver: Receiver = MidiSystem.getReceiver
  override def send(message: MidiMessage, timeStamp: Long): Unit = {
    message.getMessage match {
      case Array(SongPositionPointer, lsb, msb) ⇒
        val v = lsb | (msb << 8)
        positionUpdate(v)
        pos = v
      case Array(TimingClock) ⇒
        for {
          messages ← midi.get(pos)
          message ← messages
        } {
          receiver.send(message, -1)
        }
        positionUpdate(pos)
        pos += 1
      case Array(n, _, _) if n >= NoteOff && n <= NoteOn16 ⇒
      case Array(n, _, _) if n >= ControlChange && n <= ControlChange16 ⇒
      case Array(n, _, _) if n >= PitchBend && n <= PitchBend16 ⇒
      case Array(Stop) ⇒
        positionUpdate(0)
        pos = 0
      case _ ⇒
    }
  }
  override def close(): Unit = {
    receiver.close
  }
}

class MasterOut(receiver: MidiReceiver) extends Monad {
  private def midi(notes: Sequence): collection.Map[Int, List[MidiMessage]] = {
    val result = new mutable.HashMap[Int, ListBuffer[MidiMessage]]
    def put(time: Int, m: ShortMessage) = {
      if (!result.contains(time)) result += time → new ListBuffer[MidiMessage]
      result(time) += m
    }
    for (Value(time, duration, value, octave, accidental, attack, attackFine, release, releaseFine) ← notes.events) {
      val end = time + duration
      val midiRoot = 60
      val note = midiRoot + octave * 12 + value + accidental
      val a = attack * 8
      val r = release * 8
      put(time, new ShortMessage(ShortMessage.NOTE_ON, 0, note, a))
      put(end, new ShortMessage(ShortMessage.NOTE_OFF, 0, note, r))
    }
    result.mapValues(_.toList).toMap
  }
  def apply(input: Sequence) = {
    receiver.midi = midi(input).toMap
    Sequence.empty
  }
}
