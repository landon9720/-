package kuhn

import java.lang.Math._
import javax.sound.midi._
import collection._
import mutable.ListBuffer
import collection.mutable

// time, note, duration
case class Sequence(notes: Seq[(Int, Int, Int)], len: Int) {
  def midi: collection.Map[Int, List[MidiMessage]] = {
    val result = new mutable.HashMap[Int, ListBuffer[MidiMessage]]
    def put(time: Int, m: ShortMessage) = {
      if (!result.contains(time)) result += time → new ListBuffer[MidiMessage]
      result(time) += m
    }
    for ((time, note, d) ← notes) {
      val end = time + d
      put(time, new ShortMessage(ShortMessage.NOTE_ON, 0, 60 + note, 90))
      put(end, new ShortMessage(ShortMessage.NOTE_OFF, 0, 60 + note, 90))
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
  def apply(notes: (Int, Int, Int)*): Sequence = {
    val last = notes.maxBy(_._1)
    val len = last._1 + last._3
    new Sequence(notes, len)
  }
  val empty = Sequence(Seq.empty, 0)
}

trait Monad {
  def apply(input: Sequence): Sequence
}

object Monad {
  implicit def functionToMonad(f: Sequence ⇒ Sequence) = new Monad {
    override def apply(input: Sequence): Sequence = f(input)
  }
  implicit class MonadImplicits(m0: Monad) {
    def ===(m1: Monad): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        val s1 = m1(input)
        Sequence(s0.notes ++ s1.notes, max(s0.len, s1.len))
    }
    def >(m1: Monad): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        val s1 = m1(input)
        Sequence(s0.notes ++ s1.transposeTime(s0.len).notes, s0.len + s1.len)
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
        Sequence(s0.notes map { case (t, n, d) ⇒ (t * scale, n, d * scale) }, s0.len * scale)
    }
    def **:(scale: Int) = **(scale)
    def */(scale: Int): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        Sequence(s0.notes map { case (t, n, d) ⇒ (t / scale, n, d / scale) }, s0.len / scale)
    }
    def */:(scale: Int) = **(scale)
    def up(semitones: Int): Monad = {
      input: Sequence ⇒
        val s0 = m0(input)
        Sequence(s0.notes map { case (t, n, d) ⇒ (t, n + semitones, d) }, s0.len)
    }
    def down(semitones: Int): Monad = up(-semitones)
  }
  def rest(time: Int): Monad = {
    input: Sequence ⇒
      Sequence(Seq.empty, time)
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
      s"status=${message.getStatus.toByte.hex} length=${message.getLength} message=${message.getMessage.hex}"
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
        for {
          messages ← midi.get(pos)
          message ← messages
        } {
          MidiSystem.getReceiver.send(message, -1)
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

object MasterOut extends Monad {
  def apply(input: Sequence) = {
    R.midi = input.midi.toMap
    println(s"R.midi.size = ${R.midi.size}")
    Sequence.empty
  }
}

case class Part(sequence: Sequence) extends Monad {
  def apply(ignored: Sequence) = sequence
}

trait Song {
  implicit def sequenceToMonad(s: Sequence): Monad = Part(s)
  def S(notes: (Int, Int, Int)*): Monad = Part(Sequence(notes:_*))
  val B = 12
  def song: Monad
  def main(args: Array[String]) {
    (100 *: song) >> MasterOut apply Sequence.empty
    MidiSystem.getTransmitter.setReceiver(R)
  }
}
