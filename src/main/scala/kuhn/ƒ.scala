package kuhn

import java.lang.Math._
import javax.sound.midi._
import scala.collection.{mutable, _}
import scala.collection.mutable.ListBuffer

object ƒ {
  val beats = 12
  implicit val NoteOfScaleValue_ = NoteOfScaleValue
}
import ƒ._

trait ValueAccess[T] {
  def get(t: T): Int
  def set(t: T, value: Int): T
}

trait Event[T <: Event[T]] {
  this: T ⇒
  def time: Int
  def copyWithTime(time: Int): T
}

trait EventTime[T <: Event[T]] extends ValueAccess[Event[T]]

case class MidiEvent(time: Int, status: Int, data1: Int, data2: Int) extends Event[MidiEvent] {
  def copyWithTime(time: Int) = copy(time = time)
}

trait ValueEvent[T <: Event[T]] extends Event[T] {
  this: T ⇒
  val value: Int
  def copyWithValue(value: Int): ValueEvent[T]
}

trait EventValue[T <: Event[T]] extends ValueAccess[ValueEvent[T]]

case class Note(time: Int, value: Int, duration: Int = 1*beats, attack: Int = 128/2, release: Int = 128/2) extends ValueEvent[Note] {
  def copyWithTime(time: Int): Note = copy(time = time)
  def copyWithValue(value: Int): Note = copy(value = value)
}

object NoteTime extends EventTime[Note] {
  def get(e: Event[Note]): Int = e.time
  def set(e: Event[Note], value: Int): Event[Note] = e.copyWithTime(value)
}

object NoteValue extends ValueAccess[Note] {
  def get(e: Note): Int = e.value
  def set(e: Note, value: Int): Note = e.copyWithValue(value)
}

object NoteDuration extends ValueAccess[Note] {
  def get(e: Note): Int = e.duration
  def set(e: Note, value: Int): Note = e.copy(duration = value)
}

object NoteAttack extends ValueAccess[Note] {
  def get(e: Note): Int = e.attack
  def set(e: Note, value: Int): Note = e.copy(attack = value)
}

object NoteRelease extends ValueAccess[Note] {
  def get(e: Note): Int = e.release
  def set(e: Note, value: Int): Note = e.copy(release = value)
}

case class NoteOfScale(time: Int, value: Int, duration: Int = 1*beats, attack: Int = 128/2, release: Int = 128/2) extends ValueEvent[NoteOfScale] {
  def copyWithTime(time: Int): NoteOfScale = copy(time = time)
  def copyWithValue(value: Int): NoteOfScale = copy(value = value)
}

object NoteOfScaleTime extends EventTime[NoteOfScale] {
  def get(e: Event[NoteOfScale]): Int = e.time
  def set(e: Event[NoteOfScale], value: Int): Event[NoteOfScale] = e.copyWithTime(value)
}

object NoteOfScaleValue extends ValueAccess[NoteOfScale] {
  def get(e: NoteOfScale): Int = e.value
  def set(e: NoteOfScale, value: Int): NoteOfScale = e.copyWithValue(value)
}

object NoteOfScaleDuration extends ValueAccess[NoteOfScale] {
  def get(e: NoteOfScale): Int = e.duration
  def set(e: NoteOfScale, value: Int): NoteOfScale = e.copy(duration = value)
}

object NoteOfScaleAttack extends ValueAccess[NoteOfScale] {
  def get(e: NoteOfScale): Int = e.attack
  def set(e: NoteOfScale, value: Int): NoteOfScale = e.copy(attack = value)
}

object NoteOfScaleRelease extends ValueAccess[NoteOfScale] {
  def get(e: NoteOfScale): Int = e.release
  def set(e: NoteOfScale, value: Int): NoteOfScale = e.copy(release = value)
}

case class Chord(time: Int, value: Int, rank: Int) extends ValueEvent[Chord] {
  def copyWithTime(time: Int): Chord = copy(time = time)
  def copyWithValue(value: Int): Chord = copy(value = value)
}

object ChordTime extends EventTime[Chord] {
  def get(e: Event[Chord]): Int = e.time
  def set(e: Event[Chord], value: Int): Event[Chord] = e.copyWithTime(value)
}

object ChordValue extends ValueAccess[Chord] {
  def get(e: Chord): Int = e.value
  def set(e: Chord, value: Int): Chord = e.copyWithValue(value)
}

object ChordRank extends ValueAccess[Chord] {
  def get(e: Chord): Int = e.rank
  def set(e: Chord, value: Int): Chord = e.copy(rank = value)
}

trait Sequence[T <: Event[T]] {
  def events: Seq[T]
  def duration: Int
  def copySequence(events: Seq[T], duration: Int): Sequence[T]
  override def toString = (
    for (e ← events) yield s"$e"
  ).mkString("events\n", "\n", s"\nduration=$duration")
  def transport(delta: Int) =
    copySequence(for (e ← events) yield e.copyWithTime(delta + e.time), delta + duration)
}

object Sequence {
}

case class MidiSequence(events: Seq[MidiEvent], duration: Int) extends Sequence[MidiEvent] {
  def copySequence(events: Seq[MidiEvent], duration: Int) = copy(events = events, duration = duration)
}

object MidiSequence {
}

case class NoteSequence(events: Seq[Note], duration: Int) extends Sequence[Note] {
  def copySequence(events: Seq[Note], duration: Int) = copy(events = events, duration = duration)
}

object NoteSequence {
  def apply(notes: Note*): NoteSequence = {
    val last = notes.maxBy(_.time)
    val len = last.time + last.duration
    NoteSequence(notes, len)
  }
  def empty = new NoteSequence(Seq.empty, 0)
}

case class NoteOfScaleSequence(events: Seq[NoteOfScale], duration: Int) extends Sequence[NoteOfScale] {
  def copySequence(events: Seq[NoteOfScale], duration: Int) = copy(events = events, duration = duration)
}

object NoteOfScaleSequence {
  def apply(notes: NoteOfScale*): NoteOfScaleSequence = {
    val last = notes.maxBy(_.time)
    val len = last.time + last.duration
    NoteOfScaleSequence(notes, len)
  }
  def empty = new NoteOfScaleSequence(Seq.empty, 0)
}

case class ChordSequence(events: Seq[Chord], duration: Int) extends Sequence[Chord] {
  def copySequence(events: Seq[Chord], duration: Int) = copy(events = events, duration = duration)
}

object ChordSequence {
}

trait Monad[A <: Event[A], B <: Event[B]] {
  def apply(input: Sequence[A]): Sequence[B]
}

object Monad {
  implicit def functionToMonad[A <: Event[A], B <: Event[B]](f: Sequence[A] ⇒ Sequence[B]) = new Monad[A, B] {
    override def apply(input: Sequence[A]): Sequence[B] = f(input)
  }
  implicit class MonadImplicits[A <: Event[A], B <: Event[B]](m0: Monad[A, B]) {
    def ===(m1: Monad[A, B]): Monad[A, B] = {
      input: Sequence[A] ⇒
        val s0 = m0(input)
        val s1 = m1(input)
        s0.copySequence(s0.events ++ s1.events, max(s0.duration, s1.duration))
    }
    def >(m1: Monad[A, B]): Monad[A, B] = {
      input: Sequence[A] ⇒
        val s0 = m0(input)
        val s1 = m1(input)
        s0.copySequence(s0.events ++ s1.transport(s0.duration).events, s0.duration + s1.duration)
    }
    def >>[B2 <: Event[B2]](m1: Monad[B, B2]): Monad[A, B2] = {
      input: Sequence[A] ⇒
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
    def **(scale: Int): Monad[A, B] = {
      input: Sequence[A] ⇒
        val s0 = m0(input)
        s0.copySequence(s0.events map { e ⇒ e.copyWithTime(e.time * scale) }, s0.duration * scale)
    }
    def **:(scale: Int) = **(scale)
    def */(scale: Int): Monad[A, B] = {
      input: Sequence[A] ⇒
        val s0 = m0(input)
        s0.copySequence(s0.events map { e ⇒ e.copyWithTime(e.time / scale) }, s0.duration / scale)
    }
    def */:(scale: Int) = **(scale)
    def normalize(to: Int): Monad[A, B] = {
      input: Sequence[A] ⇒
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
  implicit class ValueMonadImplicits[A <: Event[A], B <: Event[B] : ValueAccess](m0: Monad[A, B]) {
    def up(delta: Int): Monad[A, B] = {
      input: Sequence[A] ⇒
        val s0 = m0(input)
        val accessor = implicitly[ValueAccess[B]]
        s0.copySequence(s0.events map { e ⇒ accessor.set(e, accessor.get(e) + delta) }, s0.duration)
    }
    def down(delta: Int): Monad[A, B] = up(-delta)
    def vmap(f: ValMod): Monad[A, B] = {
      input: Sequence[A] ⇒
        val s0 = m0(input)
        val accessor = implicitly[ValueAccess[B]]
        s0.copySequence(s0.events.zipWithIndex map { case (e, i) ⇒ accessor.set(e, f(i, accessor.get(e))) }, s0.duration)
    }
  }
  type ValMod = (Int, Int) ⇒ Int
  implicit class ValModImplicits(f0: ValMod) {
    def >(f1: ValMod): ValMod = {
      case (x: Int, y: Int) ⇒
        val v0 = f0(x, y)
        val v1 = f1(x, v0)
        v1
    }
  }
  object IdentityValMod extends ValMod {
    override def apply(v1: Int, v2: Int): Int = v2
  }
  case class ConstValMod(v: Int) extends ValMod {
    override def apply(v1: Int, v2: Int): Int = v
  }
  case class LinearValMod(v: Int) extends ValMod {
    override def apply(v1: Int, v2: Int): Int = v
  }
}
import kuhn.Monad._

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
      s"status=${message.getStatus.toByte.hex} length=${message.getLength} message=${message.getMessage.hex}"
    }
  }
}
import kuhn.Midi._

object R extends Receiver {
  var midi = immutable.Map.empty[Int, List[MidiMessage]]
  var pos = 0
  private val receiver: Receiver = MidiSystem.getReceiver
  override def send(message: MidiMessage, timeStamp: Long): Unit = {
    message.getMessage match {
      case Array(SongPositionPointer, lsb, msb) ⇒
        val v = lsb | (msb << 8)
        pos = v
      case Array(TimingClock) ⇒
        for {
          messages ← midi.get(pos)
          message ← messages
        } {
          receiver.send(message, -1)
        }
        pos += 1
      case _ ⇒
        println(s"message=${message.toDebugString} timeStamp=$timeStamp")
    }
  }
  override def close(): Unit = {
    println(s"close")
  }
}

object MasterOut extends Monad[Note, Note] {
  private def midi(notes: Sequence[Note]): collection.Map[Int, List[MidiMessage]] = {
    val result = new mutable.HashMap[Int, ListBuffer[MidiMessage]]
    def put(time: Int, m: ShortMessage) = {
      if (!result.contains(time)) result += time → new ListBuffer[MidiMessage]
      result(time) += m
    }
    for (Note(time, note, d, attack, release) ← notes.events) {
      val end = time + d
      put(time, new ShortMessage(ShortMessage.NOTE_ON, 0, 60 + note, attack))
      put(end, new ShortMessage(ShortMessage.NOTE_OFF, 0, 60 + note, release))
    }
    result.mapValues(_.toList).toMap
  }
  def apply(input: Sequence[Note]) = {
    R.midi = midi(input).toMap
    println(s"R.midi.size = ${R.midi.size}")
    NoteSequence.empty
  }
  MidiSystem.getTransmitter.setReceiver(R)
}

case class NotePart[T <: Event[T]](sequence: Sequence[Note]) extends Monad[T, Note] {
  def apply(ignored: Sequence[T]): Sequence[Note] = sequence
}

case class NoteOfScalePart[T <: Event[T]](sequence: Sequence[NoteOfScale]) extends Monad[T, NoteOfScale] {
  def apply(ignored: Sequence[T]) = sequence
}

case class NoteOfScaleToNotes(scale: Scale) extends Monad[NoteOfScale, Note] {
  def apply(input: Sequence[NoteOfScale]) = NoteSequence(input.events.map(scale.apply), input.duration)
}

object StartOfNotes extends Monad[Note, Note] {
  def apply(ignored: Sequence[Note]) = NoteSequence.empty
}

object StartOfNoteOfScales extends Monad[Note, NoteOfScale] {
  def apply(ignored: Sequence[Note]) = NoteOfScaleSequence.empty
}

trait Song {
  def song: Monad[Note, Note]
  def main(args: Array[String]): Unit = song >> MasterOut apply NoteSequence.empty
}

object Song {
  private def notes[T <: Event[T]](notes: Note*): Monad[T, Note] = NotePart(NoteSequence(notes:_*))
  def n(values: Int*): Monad[Note, Note] = notes(values.zipWithIndex map { case (n, i) ⇒ Note(i*beats, n) }:_*)
  def b(values: Int*): Monad[Note, Note] = new MonadImplicits[Note, Note](notes(values.zipWithIndex map { case (n, i) ⇒ Note(i*beats, n) }:_*)) N
  private def noteOfScales[T <: Event[T]](notes: NoteOfScale*): Monad[T, NoteOfScale] = NoteOfScalePart(NoteOfScaleSequence(notes:_*))
  def nn(values: Int*): Monad[NoteOfScale, NoteOfScale] = noteOfScales(values.zipWithIndex map { case (n, i) ⇒ NoteOfScale(i*beats, n) }:_*)
  def bb(values: Int*): Monad[NoteOfScale, NoteOfScale] = new MonadImplicits[NoteOfScale, NoteOfScale](noteOfScales(values.zipWithIndex map { case (n, i) ⇒ NoteOfScale(i*beats, n) }:_*)) N
}
