package maf.lattice.interfaces

import maf.core.Lattice
import spire.math.Complex

trait NumberLattice[N] extends Lattice[N] {
    self =>
    def integer: N
    def real: N

    def exclReal: N
    def exclComplex: N
    
    def inject(n: Double): N
    def inject(n: BigInt): N
    def inject(n: Complex[Double]): N
    
    def isInteger[B: BoolLattice](n: N) : B
    def isReal[B: BoolLattice](n: N) : B
    def isComplex[B: BoolLattice](n: N) : B
    
    //def isExact[B: BoolLattice](n: N) : B
    //def isInexact[B: BoolLattice](n: N) : B
    
    def ceiling(n: N): N
    def floor(n: N): N
    def round(n: N): N
    def log(n: N): N
    //def random(n: N): N
    def sin(n: N): N
    def asin(n: N): N
    def cos(n: N): N
    def acos(n: N): N
    def tan(n: N): N
    def atan(n: N): N
    def sqrt(n: N): N
    def plus(n1: N, n2: N): N
    def quotient(n1: N, n2: N): N
    def modulo(n1: N, n2: N): N
    def remainder(n1: N, n2: N): N
    def minus(n1: N, n2: N): N
    def times(n1: N, n2: N): N
    def div(n1: N, n2: N): N
    def expt(n1: N, n2: N): N
    def lt[B: BoolLattice](n1: N, n2: N): B //Less than
    def toString[S: StringLattice](n: N): S
    //def valuesBetween(n1: N, n2: N): Set[N]
    def makeString[C: CharLattice, S: StringLattice](length: N, char: C): S
    def toChar[C: CharLattice](n: N): C
    //def exactToInexact(n: N): N
    //def inexactToExact(n: N): N
    def realPart(n: N): N
    def imagPart(n: N): N

}

object NumberLattice:
    def apply[N: NumberLattice]: NumberLattice[N] = implicitly