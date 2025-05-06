package maf.language.scheme.lattices

import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, NumberLattice, RealLattice, StringLattice}
import spire.math.Complex

class ModularNumberLattice[N: NumberLattice] extends NumberLattice[(E, N)]{

    val real = (ExactLattice.top, NumberLattice[N].real)
    val integer = (ExactLattice.top, NumberLattice[N].integer)
    val exclReal = (ExactLattice.top, NumberLattice[N].exclReal)
    val exclComplex = (ExactLattice.top, NumberLattice[N].exclComplex)

    override def show(v: (E, N)): String = 
        "(" + ExactLattice.show(v._1) + ", " + NumberLattice[N].show(v._2) + ")"

    override def bottom: (E, N) =
        (ExactLattice.bottom, NumberLattice[N].bottom)

    override def top: (E, N) =
        (ExactLattice.top, NumberLattice[N].top)

    override def eql[B: BoolLattice](x: (E, N), y: (E, N)): B =
        NumberLattice[N].eql[B](x._2, y._2)

    override def join(x: (E, N), y: => (E, N)): (E, N) =
        (ExactLattice.join(x._1, y._1), NumberLattice[N].join(x._2, y._2))
    
    def isExactInt[B: BoolLattice](n: (E, N)): B =
        BoolLattice[B].join(ExactLattice.isExact[B](n._1), NumberLattice[N].isInteger[B](n._2))
        
    def inject(n: BigInt): (E, N) = (Exact, NumberLattice[N].inject(n))
    def inject(n: Double): (E, N) = (Exact, NumberLattice[N].inject(n))
    def inject(n: Complex[Double]): (E, N) = (Exact, NumberLattice[N].inject(n))
    def inject(n: N): (E, N) = (Exact, n)

    def subsumes(x: (E, N), y: => (E, N)): Boolean =
        ExactLattice.subsumes(x._1, y._1) && NumberLattice[N].subsumes(x._2, y._2)

    def isInteger[B: BoolLattice](n: (E, N)): B = NumberLattice[N].isInteger[B](n._2)
    def isReal[B: BoolLattice](n: (E, N)): B = NumberLattice[N].isReal[B](n._2)
    def isComplex[B: BoolLattice](n: (E, N)): B = NumberLattice[N].isComplex[B](n._2)

    def toString[S: StringLattice](n: (E, N)): S =
        NumberLattice[N].toString[S](n._2)

    def quotient(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.minus(n1._1, n2._1), NumberLattice[N].minus(n1._2, n2._2))

    override def plus(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.plus(n1._1, n2._1), NumberLattice[N].plus(n1._2, n2._2))

    override def minus(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.minus(n1._1, n2._1), NumberLattice[N].minus(n1._2, n2._2))

    override def times(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.times(n1._1, n2._1), NumberLattice[N].times(n1._2, n2._2))

    override def sqrt(n: (E, N)): (E, N) =
        (ExactLattice.sqrt(n._1), NumberLattice[N].sqrt(n._2))

    def lt[B: BoolLattice](n1: (E, N), n2: (E, N)): B =
        NumberLattice[N].lt(n1._2, n2._2)

    def exactToInexact(n: (E, N)): (E, N) =
        (ExactLattice.exactToInexact(n._1), n._2)

    def inexactToExact(n: (E, N)): (E, N) =
        (ExactLattice.inexactToExact(n._1), n._2)    

    override def acos(n: (E, N)): (E, N) =
        (ExactLattice.acos(n._1), NumberLattice[N].acos(n._2))

    override def asin(n: (E, N)): (E, N) =
        (ExactLattice.asin(n._1), NumberLattice[N].asin(n._2))

    override def atan(n: (E, N)): (E, N) =
        (ExactLattice.atan(n._1), NumberLattice[N].atan(n._2))

    override def cos(n: (E, N)): (E, N) =
        (ExactLattice.cos(n._1), NumberLattice[N].cos(n._2))

    def floor(n: (E, N)): (E, N) =
        (ExactLattice.floor(n._1), NumberLattice[N].floor(n._2))

    def ceiling(n: (E, N)): (E, N) =
        (ExactLattice.ceiling(n._1), NumberLattice[N].ceiling(n._2))

    override def sin(n: (E, N)): (E, N) =
        (ExactLattice.sin(n._1), NumberLattice[N].sin(n._2))

    override def tan(n: (E, N)): (E, N) =
        (ExactLattice.tan(n._1), NumberLattice[N].tan(n._2))

    def round(n: (E, N)): (E, N) =
        (ExactLattice.round(n._1), NumberLattice[N].round(n._2))

    override def div(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.div(n1._1, n2._1), NumberLattice[N].div(n1._2, n2._2))

    override def log(n: (E, N)): (E, N) =
        (ExactLattice.log(n._1), NumberLattice[N].log(n._2))

    override def expt(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.expt(n1._1, n2._1), NumberLattice[N].expt(n1._2, n2._2))

    def modulo(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.modulo(n1._1, n2._1), NumberLattice[N].modulo(n1._2, n2._2))

    def remainder(n1: (E, N), n2: (E, N)): (E, N) =
        (ExactLattice.remainder(n1._1, n2._1), NumberLattice[N].remainder(n1._2, n2._2))

    def isExact[B: BoolLattice](n: (E, N)): B = ExactLattice.isExact[B](n._1)
    def isInexact[B: BoolLattice](n: (E, N)): B = ExactLattice.isInexact[B](n._1)

    override def toChar[C: CharLattice](n: (E, N)): C = NumberLattice[N].toChar[C](n._2)

    override def imagPart(n: (E, N)): (E, N) = (n._1, NumberLattice[N].imagPart(n._2))
    override def realPart(n: (E, N)): (E, N) = (n._1, NumberLattice[N].realPart(n._2))

    override def makeString[C: CharLattice, S: StringLattice](length: (E, N), char: C): S = NumberLattice[N].makeString[C, S](length._2, char)
    
  
}
