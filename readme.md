Scala Language MIDI Sequencer
=============================

This project is:

* an experiment in creating music by building monadic transformations on sequences of value data
* a programable instrument that generates MIDI note events
* a Scala toolchain for MIDI sequencing
* a text file drum machine

References
----------

https://www.midi.org/specifications/item/table-2-expanded-messages-list-status-bytes
http://www.electronics.dit.ie/staff/tscarff/Music_technology/midi/midi_note_numbers_for_octaves.htm
http://abcnotation.com/wiki/abc:standard:v2.1

Dimensions
----------

time
  midi note
  note
  note of scale
  chord
value
  midi note
  note
  note of scale
  chord root and ranks
duration
  midi note
  note
  note of scale
attack/release
  midi note
  note
  note of scale
accidental
  note of scale

matrix
------

rows of cols of integers


column is always time
row semantic is provided by context
value
  a-f literal value
  0-9 literal value
  reference
  any char / semantic by context

song:
+0,0-------------------
|a

every file defines 1 song matrix
the song matrix is used to yield midi notes
the minimum viable data is (time,#) pairs
therefore a t*1 matrix can only be used 1 way

a a e e f f e   d d c c b b a

song:
+0,0---------------------------------------------
|a a e e f f e   d d c c b b a
|1 1 1 1 1 1 2   1 1 1 1 1 1 2

song is a t*2 matrix
what does it do with the x dimension?
 x=0 # value
 x=1 duration
 x=2 attack
 x=3 release

absolute pitch = midi note number - middle c note number

triad+0,0
     |111
     |135
     |000

VII:chordspec
   +0,0-
   |1111
   |1357
   |000-

song    +0,0--------------------------
#       |a a e e f f e   d d c c b b a
duration|1 1 1 1 1 1 2   1 1 1 1 1 1 2
triad   |1   1   2   3   1   1 2     1



