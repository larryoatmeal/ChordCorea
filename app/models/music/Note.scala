package models.music

import play.api.Logger
import models.utility._

case class MidiNote(midi: Integer){

  def transpose(semitones: Integer) = {
    MidiNote(midi + semitones)
  }

}

case class DisplayNote(letter: NoteLetters.Value, sharpFlat: SharpFlats.Value)

case class DisplayNoteWithOctave(displayNote: DisplayNote, octave: Integer){

  val midify = {
    val base = NoteUtils.letterToBaseMidi(displayNote.letter) 
    val offset = NoteUtils.sharpFlatToOffset(displayNote.sharpFlat)
    MidiNote(base + offset + NoteUtils.octaveOffset(octave))
  }
}

object NoteUtils{//parses notes into DisplayNote and DisplayNoteWithOctave

  //Parsing
  val letterMap = Map(
    "A" -> NoteLetters.A, 
    "B" -> NoteLetters.B, 
    "C" -> NoteLetters.C,
    "D" -> NoteLetters.D,
    "E" -> NoteLetters.E,
    "F" -> NoteLetters.F,
    "G" -> NoteLetters.G,
    "a" -> NoteLetters.A, 
    "b" -> NoteLetters.B, 
    "c" -> NoteLetters.C,
    "d" -> NoteLetters.D,
    "e" -> NoteLetters.E,
    "f" -> NoteLetters.F,
    "g" -> NoteLetters.G
  )

  val sharpFlatMap = Map(
    "##" -> SharpFlats.DoubleSharp,
    "bb" -> SharpFlats.DoubleFlat,
    "b" -> SharpFlats.Flat,
    "#" -> SharpFlats.Sharp,
    "N" -> SharpFlats.Natural,
    "n" -> SharpFlats.Natural
  )

  val letterToBaseMidi = Map(
   NoteLetters.C -> 0,
   NoteLetters.D -> 2,
   NoteLetters.E -> 4,
   NoteLetters.F -> 5,
   NoteLetters.G -> 7,
   NoteLetters.A -> 9,
   NoteLetters.B -> 11
  )


  val sharpFlatToOffset = Map(
     SharpFlats.Flat -> -1,
     SharpFlats.Sharp -> 1,
     SharpFlats.DoubleFlat -> -2,
     SharpFlats.DoubleSharp -> 2,
     SharpFlats.Nothing -> 0,
     SharpFlats.Natural -> 0
    )

  val offsetToSharpFlat = sharpFlatToOffset.map(_.swap)

  val numberOfLetters = 7
  val numberOfNotes = 12
  val letterNumberMap = Map(
    NoteLetters.A -> 0,
    NoteLetters.B -> 1,
    NoteLetters.C -> 2,
    NoteLetters.D -> 3,
    NoteLetters.E -> 4,
    NoteLetters.F -> 5,
    NoteLetters.G -> 6
    )

  val numberLetterMap = letterNumberMap.map(_.swap)


  // n for natural
  val pattern = "^([ABCDEFGabcdefg])(##|bb|[b#Nn])?$".r

  def parse(str: String):Option[DisplayNote] = {
    str match {
      case pattern(letter, null) => {
        Some(DisplayNote(letterMap(letter), SharpFlats.Nothing))
      }
      case pattern(letter, suffix) => {
        Some(DisplayNote(letterMap(letter), sharpFlatMap(suffix)))
      }
      case _ => {
        Logger.error("INVALID INPUT")
        None
      }
    }
  }

  val patternOctave = "(.*?)(-?[0-9]{1,2})$".r //.*? for lazy, instead of greedy. Match any integer with two digits. Pos/neg

  def parseWithOctave(str: String):Option[DisplayNoteWithOctave] = {
    str match {
      case patternOctave(before, number) => {
        parse(before) match {
          case Some(displayNote) => {
            Some(DisplayNoteWithOctave(displayNote, number.toInt))//toInt guaranteed to work by pattern matching
          }
          case _ => {
            None
          }
        }
      }
      case _ => {
        None
      }
    }
  }

  //MIDI conversion









  def octaveOffset(octave: Integer) = {
    (octave + 1) * 12 //C4 is 60
  }


  // def closestOctaveToNote(midiNote: MidiNote):Integer= {
  //   val floored = (midiNote.midi / 12) * 12
  //   val ceiled = (midiNote.midi / 12 + 1) * 12 

  //   if(math.abs(floored - midiNote.midi) < math.abs(ceiled - midiNote.midi)){
  //     floored
  //   }
  //   else{
  //     ceiled
  //   }

  // }


  def closestNeighborMod(src: Integer, dst: Integer, n: Integer):Integer = {
    val m = MyMath.mod(dst-src,n)
    if(m > n/2){
      m - n
    }
    else{
      m
    }
  }


  def noteFromMidi(midiNote: MidiNote, letter: NoteLetters.Value):DisplayNoteWithOctave = {//inverse of midify
    val base = letterToBaseMidi(letter)
    val offset = closestNeighborMod(base, midiNote.midi % 12, 12)

    if(offsetToSharpFlat.contains(offset)){
      val sharpFlat = offsetToSharpFlat(offset)
      val octaves = midiNote.midi / 12 - 1
      DisplayNoteWithOctave(DisplayNote(letter, sharpFlat),octaves)
    }
    else{//Sometimes impossible. For example, tranpose Cbb down by M6. Should be Ebbb. But that's just stupid

      Logger.warn(s"INVALID NOTE FROM MIDI:$midiNote,$letter, try again")

      //if too high, try going to the letter above
      if(offset > 2){
        noteFromMidi(midiNote, numberLetterMap(MyMath.mod(letterNumberMap(letter) + 1, 7)))
      }
      else{//if too low, try going to the letter below
        noteFromMidi(midiNote, numberLetterMap(MyMath.mod(letterNumberMap(letter) - 1, 7)))
      }
    }

  }

  def testNoteToMidi(){
    val tests = List((60, NoteLetters.C),(61,NoteLetters.C),(62, NoteLetters.C),(60, NoteLetters.B),(55, NoteLetters.G))

    tests.foreach{
      case (midi, letter) =>{
        val note = noteFromMidi(MidiNote(midi),letter)
        Logger.debug(note.toString)
      }
    }
  }


}



