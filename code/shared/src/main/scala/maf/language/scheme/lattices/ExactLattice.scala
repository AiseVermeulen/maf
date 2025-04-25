package maf.language.scheme.lattices

import maf.lattice.interfaces.{BoolLattice, NumberLattice}
import spire.math.Complex

sealed trait E

case object exact extends E
case object inexact extends E
case object exactLatticeTop extends E
case object exactLatticeBottom extends E

class ExactLattice extends NumberLattice[E]{

    override def show(v: E): String =
        v match
            case exact => "exact"
            case inexact => "inexact"
            case top => "top"
            case bottom => "bottom"

    override def inject(n: BigInt): E = exact

    override def inject(n: Double): E = top //inexact

    override def inject(n: Complex[Double]): E = exact

    override def bottom: E = exactLatticeBottom

    override def top: E = exactLatticeTop

    override def join(x: E, y: => E): E =
        if subsumes(x, y) then
            x
        else if subsumes(y, x) then
            y
        else 
            top    
            
    
    def toInt(n: E): E = n

    def lt[B: BoolLattice](n1: E, n2: E): B = BoolLattice[B].top //fout
    override def eql[B: BoolLattice](n1: E, n2: E): B = BoolLattice[B].top //fout
    
    private def contaminate(n1: E, n2: E): E =
        n1 match
            case inexact => inexact
            case top => top
            case exact =>
                n2 match
                    case inexact => inexact
                    case exact => exact
                    case top => top
                    

    override def subsumes(x: E, y: => E): Boolean =
        if x == exactLatticeTop then
            true
        else if x == exactLatticeBottom then
            isBottom(y)
        else if x == exact then
            y == bottom || y == exact
        else
            y == bottom || y == inexact
        /*
        x match
            case exactLatticeTop => true
            case bottom => isBottom(y)
            case exact => y == top || y == exact
            case inexact => y == top || y == inexact

         */
    override def plus(n1: E, n2: E): E =
        contaminate(n1, n2)

    override def minus(n1: E, n2: E): E =
        contaminate(n1, n2)

    override def times(n1: E, n2: E): E =
        contaminate(n1, n2)

    def quotient(n1: E, n2: E): E =
        contaminate(n1, n2)    

    override def sqrt(n: E): E =
        n match
            case inexact => inexact
            case _ => top

    def exactToInexact(n: E): E =
        inexact

    def toReal(n: E): E = n

    def inexactToExact(n: E): E = exact

    def ceiling(n: E): E = n

    def floor(n: E): E = n

    //override def random(n: E): E = exact

    def round(n: E): E = n

    override def expt(n1: E, n2: E): E = contaminate(n1, n2)

    override def acos(n: E): E = top

    override def atan(n: E): E = top

    override def cos(n: E): E = top

    override def asin(n: E): E = top

    override def sin(n: E): E = top

    override def tan(n: E): E = top

    override def log(n: E): E = top //nog aanpassen

    def modulo(n1: E, n2: E): E = contaminate(n1, n2)

    override def div(n1: E, n2: E): E = top /// nog aanpassen

    def remainder(n1: E, n2: E): E = contaminate(n1, n2)
}
