package kuhn

import kuhn.Notes._

import scala.collection.mutable.ArrayBuffer

object  Notes {
  private var x = -60
  val C0 = x; x+=1
  val Cs0 = x; x+=1
  val D0 = x; x+=1
  val Ds0 = x; x+=1
  val E0 = x; x+=1
  val F0 = x; x+=1
  val Fs0 = x; x+=1
  val G0 = x; x+=1
  val Gs0 = x; x+=1
  val A = x; x+=1
  val Bba = x; x+=1
  val Ba = x; x+=1
  val C1 = x; x+=1
  val Cs1 = x; x+=1
  val D1 = x; x+=1
  val Ds1 = x; x+=1
  val E1 = x; x+=1
  val F1 = x; x+=1
  val Fs1 = x; x+=1
  val G1 = x; x+=1
  val Gs1 = x; x+=1
  val A2 = x; x+=1
  val Bb2 = x; x+=1
  val B2 = x; x+=1
  val C2 = x; x+=1
  val Cs2 = x; x+=1
  val D2 = x; x+=1
  val Ds2 = x; x+=1
  val E2 = x; x+=1
  val F2 = x; x+=1
  val Fs2 = x; x+=1
  val G2 = x; x+=1
  val Gs2 = x; x+=1
  val A3 = x; x+=1
  val Bb3 = x; x+=1
  val B3 = x; x+=1
  val C3 = x; x+=1
  val Cs3 = x; x+=1
  val D3 = x; x+=1
  val Ds3 = x; x+=1
  val E3 = x; x+=1
  val F3 = x; x+=1
  val Fs3 = x; x+=1
  val G3 = x; x+=1
  val Gs3 = x; x+=1
  val A4 = x; x+=1
  val Bb4 = x; x+=1
  val B4 = x; x+=1
  val C4 = x; x+=1
  val Cs4 = x; x+=1
  val D4 = x; x+=1
  val Ds4 = x; x+=1
  val E4 = x; x+=1
  val F4 = x; x+=1
  val Fs4 = x; x+=1
  val G4 = x; x+=1
  val Gs4 = x; x+=1
  val A5 = x; x+=1
  val Bb5 = x; x+=1
  val B5 = x; x+=1
  val C5 = x; x+=1
  val Cs5 = x; x+=1
  val D5 = x; x+=1
  val Ds5 = x; x+=1
  val E5 = x; x+=1
  val F5 = x; x+=1
  val Fs5 = x; x+=1
  val G5 = x; x+=1
  val Gs5 = x; x+=1
  val A6 = x; x+=1
  val Bb6 = x; x+=1
  val B6 = x; x+=1
  val C6 = x; x+=1
  val Cs6 = x; x+=1
  val D6 = x; x+=1
  val Ds6 = x; x+=1
  val E6 = x; x+=1
  val F6 = x; x+=1
  val Fs6 = x; x+=1
  val G6 = x; x+=1
  val Gs6 = x; x+=1
  val A7 = x; x+=1
  val Bb7 = x; x+=1
  val B7 = x; x+=1
  val C7 = x; x+=1
  val Cs7 = x; x+=1
  val D7 = x; x+=1
  val Ds7 = x; x+=1
  val E7 = x; x+=1
  val F7 = x; x+=1
  val Fs7 = x; x+=1
  val G7 = x; x+=1
  val Gs7 = x; x+=1
  val A8 = x; x+=1
  val Bb8 = x; x+=1
  val B8 = x; x+=1
  val C8 = x; x+=1
  val Cs8 = x; x+=1
  val D8 = x; x+=1
  val Ds8 = x; x+=1
  val E8 = x; x+=1
  val F8 = x; x+=1
  val Fs8 = x; x+=1
  val G8 = x; x+=1
  val Gs8 = x; x+=1
  val A9 = x; x+=1
  val Bb9 = x; x+=1
  val B9 = x; x+=1
  val C9 = x; x+=1
  val Cs9 = x; x+=1
  val D9 = x; x+=1
  val Ds9 = x; x+=1
  val E9 = x; x+=1
  val F9 = x; x+=1
  val Fs9 = x; x+=1
  val G9 = x; x+=1
  val Gs9 = x; x+=1
  val A10 = x; x+=1
  val Bb10 = x; x+=1
  val B10 = x; x+=1
  val C10 = x; x+=1
  val Cs10 = x; x+=1
  val D10 = x; x+=1
  val Ds10 = x; x+=1
  val E10 = x; x+=1
  val F10 = x; x+=1
  val Fs10 = x; x+=1
  val G10 = x; x+=1
  val Gs10 = x; x+=1
//
//  implicit object IntNote extends Note[Int] {
//    def intVal(a: Int) = a
//  }

//  val N0 = NoteOfScale(0)
//  val N1 = NoteOfScale(1)
//  val N2 = NoteOfScale(2)
//  val N3 = NoteOfScale(3)
//  val N4 = NoteOfScale(4)
//  val N5 = NoteOfScale(5)
//  val N6 = NoteOfScale(6)
//  val N7 = NoteOfScale(7)
//  val N8 = NoteOfScale(8)
//  val N9 = NoteOfScale(9)
//  val N10 = NoteOfScale(10)
//  val N11 = NoteOfScale(11)
//  val N12 = NoteOfScale(12)
}

//trait Note[A] {
//  def intVal(a: A): Int
//}


//
//case class NoteOfScale(rank: Int) {
//  def unary_- = NoteOfScale(-rank)
//}

abstract class Scale(steps: String) {
  val root: Int
  val ranks: ArrayBuffer[Int] = {
    var i = 0
    val r = ArrayBuffer(0)
    steps.foreach {
      case 'w' ⇒
        i += 2
        r += i
      case 'h' ⇒
        i += 1
        r += i
    }
    r
  }
  def apply(n: NoteOfScale): Note = {
    var rank = n.value
    var o = 0
    while (rank < 0) {
      rank += ranks.size
      o -= 1
    }
    while (rank >= ranks.size) {
      rank -= ranks.size
      o += 1
    }
    Note(n.time, ranks(rank) + o * 12 + n.accidental, n.duration, n.attack, n.release)
  }
}

case class MajorScale(root: Int) extends Scale("wwhwww")
case class MinorScale(root: Int) extends Scale("whwwhw")
