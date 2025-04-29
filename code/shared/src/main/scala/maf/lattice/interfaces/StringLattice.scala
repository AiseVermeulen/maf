package maf.lattice.interfaces

import maf.core._

case object NotANumberString extends Error

/** A lattice for strings */
trait StringLattice[S] extends Lattice[S]:
    def inject(s: String): S
    def length[N: NumberLattice](s: S): N
    def append(s1: S, s2: S): S
    // TODO[easy]: in practice, substring can result in an error and should use MayFail
    def substring[N: NumberLattice](
        s: S,
        from: N,
        to: N
      ): S
    // TODO[easy]: in practice, ref can result in an error and should use MayFail
    def ref[N: NumberLattice, C: CharLattice](s: S, i: N): C
    // TODO[easy]: in practice, set can result in an error and should use MayFail
    def set[N: NumberLattice, C: CharLattice](
        s: S,
        i: N,
        c: C
      ): S
    def lt[B: BoolLattice](s1: S, s2: S): B
    def toSymbol[Sym: SymbolLattice](s: S): Sym
    def toNumber[N: NumberLattice](s: S): MayFail[N, Error]

object StringLattice:
    def apply[S: StringLattice]: StringLattice[S] = implicitly
