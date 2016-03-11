package org.compevol.apg

final case class Nucleotide(val state: Int) extends AnyVal with State

object Nucleotide extends DataType[Nucleotide] {

  val A = Nucleotide(0)
  val C = Nucleotide(1)
  val G = Nucleotide(2)
  val T = Nucleotide(3)
  val U = T
  val R = Nucleotide(4)
  val Y = Nucleotide(5)
  val M = Nucleotide(6)
  val W = Nucleotide(7)
  val S = Nucleotide(8)
  val K = Nucleotide(9)
  val B = Nucleotide(10)
  val D = Nucleotide(11)
  val H = Nucleotide(12)
  val V = Nucleotide(13)
  val N = Nucleotide(14)
  override val unknown = Nucleotide(15)
  override val gap = Nucleotide(16)

  def apply(c: Char): Nucleotide = char2nuc(c)

  override val stateCount: Int = 4

  override val ambiguousStateCount: Int = 17

  val char2nuc = Map(
    'A' -> A,
    'C' -> C,
    'G' -> G,
    'T' -> T,
    'U' -> U,
    'R' -> R,
    'Y' -> Y,
    'M' -> M,
    'W' -> W,
    'S' -> S,
    'K' -> K,
    'B' -> B,
    'D' -> D,
    'H' -> H,
    'V' -> V,
    'N' -> N,
    '?' -> unknown,
    '-' -> gap
  )

  val nuc2char = char2nuc map (_.swap)

  val ambiguities = Map(
    A -> Set(A),
    C -> Set(C),
    G -> Set(G),
    T -> Set(T),
    U -> Set(U),
    R -> Set(A, G),
    Y -> Set(C, T),
    M -> Set(A, C),
    W -> Set(A, T),
    S -> Set(C, G),
    K -> Set(G, T),
    B -> Set(C, G, T),
    D -> Set(A, G, T),
    H -> Set(A, C, T),
    V -> Set(A, C, G),
    N -> Set(A, C, G, T),
    unknown -> Set(A, C, G, T),
    gap -> Set(A, C, G, T)
  )

  val validChars: Set[Char] = char2nuc.keySet

  override val validStrings: Set[String] = validChars.map (_.toString)

  override val states: Vector[Nucleotide] = Vector(A, C, G, T)

  override def unambiguous(s: Nucleotide): Set[Nucleotide] = ambiguities(s)

  override def apply(s: String): Nucleotide = {
    require(s.length == 1)
    apply(s(0))
  }

  def toChar(s: Nucleotide): Char = nuc2char(s)

  override def toString(s: Nucleotide): String = toChar(s).toString

}
