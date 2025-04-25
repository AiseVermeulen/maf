package maf.language.scheme.lattices

import maf.lattice.interfaces.{BoolLattice, IntLattice, NumberLattice, RealLattice}
import spire.math.Complex

class ModularNumberLattice[I: IntLattice, R: RealLattice, Comp: NumberLattice] extends NumberLattice[(E, (I, R, Comp))]{
    
    type N = (E, (I, R, Comp))
    
    def EL = new ExactLattice
    def NTL = NumericTowerLattice[I, R, Comp]

    override def show(v: (E, (I, R, Comp))): String = 
        "(" + EL.show(v._1) + ", " + NTL.show(v._2) + ")"

    override def bottom: (E, (I, R, Comp)) =
        (EL.bottom, NTL.bottom)

    override def top: (E, (I, R, Comp)) =
        (EL.top, NTL.top)

    override def eql[B: BoolLattice](x: (E, (I, R, Comp)), y: (E, (I, R, Comp))): B =
        NumericTowerLattice[I, R, Comp].eql[B](x._2, y._2)

    override def join(x: (E, (I, R, Comp)), y: => (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.join(x._1, y._1), NTL.join(x._2, y._2))    
    

    override def inject(n: BigInt): N = (exact, NumericTowerLattice[I, R, Comp].inject(n))
    override def inject(n: Double): N = (exact, NumericTowerLattice[I, R, Comp].inject(n))
    override def inject(n: Complex[Double]): N = (exact, NumericTowerLattice[I, R, Comp].inject(n))

    override def subsumes(x: N, y: => N): Boolean =
        EL.subsumes(x._1, y._1) && NumericTowerLattice[I, R, Comp].subsumes(x._2, y._2)

    def ItoModularNumber(i: I): N = (exact, (i, RealLattice[R].bottom, NumberLattice[Comp].bottom))
    def RtoModularNumber(r: R): N = (exact, (IntLattice[I].bottom, r, NumberLattice[Comp].bottom))
    
    def getI(n: N) : I = n._2._1
    def getR(n: N) : R = n._2._2

    def quotient(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.minus(n1._1, n2._1), NTL.minus(n1._2, n2._2))
        
    def toReal(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (n._1, NTL.toReal(n._2))

    override def plus(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.plus(n1._1, n2._1), NTL.plus(n1._2, n2._2))

    override def minus(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.minus(n1._1, n2._1), NTL.minus(n1._2, n2._2))

    override def times(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.times(n1._1, n2._1), NTL.times(n1._2, n2._2))

    override def sqrt(n: N): N =
        (EL.sqrt(n._1), NumericTowerLattice[I, R, Comp].sqrt(n._2))

    def lt[B: BoolLattice](n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): B =
        NumericTowerLattice[I, R, Comp].lt(n1._2, n2._2)

    def exactToInexact(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.exactToInexact(n._1), n._2)

    def inexactToExact(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.inexactToExact(n._1), n._2)    

    override def acos(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.acos(n._1), NumericTowerLattice[I, R, Comp].acos(n._2))

    override def asin(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.asin(n._1), NumericTowerLattice[I, R, Comp].asin(n._2))

    override def atan(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.atan(n._1), NumericTowerLattice[I, R, Comp].atan(n._2))

    override def cos(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.cos(n._1), NumericTowerLattice[I, R, Comp].cos(n._2))

    def floor(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.floor(n._1), NumericTowerLattice[I, R, Comp].floor(n._2))

    def ceiling(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.ceiling(n._1), NumericTowerLattice[I, R, Comp].ceiling(n._2))

    override def sin(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.sin(n._1), NumericTowerLattice[I, R, Comp].sin(n._2))

    override def tan(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.tan(n._1), NumericTowerLattice[I, R, Comp].tan(n._2))

    def round(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.round(n._1), NumericTowerLattice[I, R, Comp].round(n._2))

    override def div(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.div(n1._1, n2._1), NTL.div(n1._2, n2._2))

    override def log(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.log(n._1), NumericTowerLattice[I, R, Comp].log(n._2))

    override def expt(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.expt(n1._1, n2._1), NTL.expt(n1._2, n2._2))

    def modulo(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.modulo(n1._1, n2._1), NTL.modulo(n1._2, n2._2))

    def remainder(n1: (E, (I, R, Comp)), n2: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.remainder(n1._1, n2._1), NTL.remainder(n1._2, n2._2))

    def toInt(n: (E, (I, R, Comp))): (E, (I, R, Comp)) =
        (EL.toInt(n._1), NumericTowerLattice[I, R, Comp].toInt(n._2))    
}
