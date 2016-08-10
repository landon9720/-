Scala Language MIDI Sequencer
=============================

This project is:

* an experiment in creating music by building monadic transformations on sequences of value data
* a programable instrument that generates MIDI note events
* a Scala toolchain for MIDI sequencing
* a text file drum machine

Getting Started
---------------

Build:

```
sbt start-script
```

Run:

```
target/start kuhn.UI
```

References
----------

* <https://www.midi.org/specifications/item/table-2-expanded-messages-list-status-bytes>
* <http://www.electronics.dit.ie/staff/tscarff/Music_technology/midi/midi_note_numbers_for_octaves.htm>
* <http://abcnotation.com/wiki/abc:standard:v2.1>
* <https://github.com/mabe02/lanterna>

Dimensions
----------

* time
  * midi note
  * note
  * note of scale
  * chord
* value
  * midi note
  * note
  * note of scale
  * chord root and ranks
* duration
  * midi note
  * note
  * note of scale
* attack/release
  * midi note
  * note
  * note of scale
* accidental
  * note of scale
  * chord root and ranks

Matrix
------

* rows of cols of integers _no_
* rows col cols of unicode characters
* column is always time _no_
* X axis
  * beat matrix: beat = X * scale
  * mapping matrix: input = X
* row semantic is provided by context _no, not really_
* value
  * a-f literal value - accidentals on own row as f(lat) n(atural) s(sharp) - cannot represent coincident notes
  * 0-9 literal value
  * reference
  * any char / semantic by context
  * an instance of a class

```
song:
+0,0-------------------
|a
```

every file defines 1 song matrix
a beat matrix may be configured as "start here"
the song matrix is used to yield midi notes
the minimum viable data is (time,#) pairs
therefore a t*1 matrix can only be used 1 way

```
a a e e f f e   d d c c b b a

song:
+0,0---------------------------------------------
|a a e e f f e   d d c c b b a
|1 1 1 1 1 1 2   1 1 1 1 1 1 2
```

if song is a t*2 matrix?
what does it do with the x dimension?
 * x=0 # value
 * x=1 duration
 * x=2 attack
 * x=3 release
 * x=? accidental

absolute pitch = midi note number - middle c note number

```
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
```

matrix can be interpreterted as
* note
* degree of scale
* chord
* voice
* scale

```
note   cdefgabcdefgabc
octave 444444455555556
attack aaaaaaaaaaaaaaa
atfine 000000001122334
release
refine

       0         1
       012345678901
      +------------
scale |101011010101

             +0,0------------
note         |012345601234560
accidental   |             sf
octave       |444444455555556


             +0,0--
rankofscale  |10101
accidental   |    f

voice        +0,0---
value0       |1 1  1
value1       |1  1 1
value2       |1   11

song         +0,0----
             |1415141

triad+-----
     |10101

song         +0,0-----
triad        |1415141
M7           |       5
```

A Beat Matrix provides a timeseries of values.

* A value may be defined in 1-line mode using value C-B, octave, accidental
* A value may be defined in manyline mode using value_n rows
* 1-line and manyline mode may be used in the same matrix; a matrix may produce multiple simultaneous voices

Value Is A or B:

 A. Something that can be mapped to an absolute note

    - A MIDI note number
    - A C4=0 note number (MIDI note number - 60)
    - A mapped note

      1. A degree of scale
      2. A degree of chord (which yields a degree of scale)
      3. A degree of n - what is n?

B. Something that can stand for some other sequence of values

    - The value used is passed in and can affect values defined therein

      1. Y offset
      2. transpose value (v=v+input_v)

    - The values produced by the referenced matrix are triggered

      1. it is a forked line values, concurrent in time to the higher level matrix

A matrix defines a set of values in time.

* what is the meaning of the Y axis?
* related to value? thinking not. thinking so.
* no meaning at all?
* meaning in labels/options
* Y value can represent degree of chord for defining chord voicing

Mapping Matrix (singleton):

```
                  0         1         2         3         4         5         6         7
                  01234567890123456789012345678901234567890123456789012345678901234567890
                 +--------------------------------------------------------------------------
ionian           |1010110101011
triad            |10101
7                |1010101
```

Mapping Matrix Options:

* type ___ (scale or chord)
* chord only: in reference to scale ___

Beat Matrix (class):

```
                  0         1         2         3         4         5         6         7
                  01234567890123456789012345678901234567890123456789012345678901234567890
hello my name is +--------------------------------------------------------------------------
duration         |
value            |
octave           |
accidental       |
attack           |
attack_fine      |
release          |
release_fine     |
value_n          |
value_n+1        |
value_n+...      |
value_n+m        |
ref_matrix_1     |
ref_matrix_2     |
ref_matrix_...   |
// i don't really|have a comment here, but I could
```

Options:

* name of matrix
* X scale
  * 1/2 to 1/12 scale for longer/less resolution
  * 2x to 12x for short/precise
* n = value_by_Y offset
* m = value_by_Y count
* show/hide rows
* add/remove reference rows
* start here y/n

for all value rows (value and reference rows): mode:

value mode: 1-line mode, many-line mode (value mode is set per value row)

In many-line mode: also choose meaning of cell:
  1. duration, attack, attack fine, release, release fine
  2. for matrix references: cell value is passed-in and available as ø

A matrix defines a timeseries of notes using a variety of abstractions















