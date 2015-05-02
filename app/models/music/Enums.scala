package models.music

object NoteLetters extends Enumeration {
  type NoteLetters = Value
  val A, B, C, D, E, F, G= Value
}

object SharpFlats extends Enumeration {
  type SharpFlats = Value
  val Sharp, Flat, DoubleFlat, DoubleSharp, Natural, Nothing = Value
}

object IntervalQualities extends Enumeration {
  type IntervalQualities = Value
  val m, M, P, A, d, AA, dd, AAA, ddd, AAAA, dddd = Value //Includes doubly, triply, and quadruply augmented and diminished
}