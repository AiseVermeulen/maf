package maf.language.scheme.lattices

import maf.core.Lattice
import maf.lattice.interfaces.{BoolLattice, NumberLattice, StringLattice}
import spire.math.Complex

sealed trait E

case object Exact extends E
case object Inexact extends E
case object Top extends E
case object Bottom extends E

object ExactLattice extends Lattice[E]{

    override def show(v: E): String =
        v match
            case Exact => "exact"
            case Inexact => "inexact"
            case Top => "top"
            case Bottom => "bottom"

    def inject(n: BigInt): E = Exact
    def inject(n: Double): E = Top //inexact
    def inject(n: Complex[Double]): E = Exact

    def bottom: E = Bottom
    def top: E = Top
            
    def isExact[B: BoolLattice](e: E): B =
        e match
            case Top => BoolLattice[B].top
            case Bottom => BoolLattice[B].bottom
            case _ => BoolLattice[B].inject(e == Exact)

    def isInexact[B: BoolLattice](e: E): B =
        e match
            case Top => BoolLattice[B].top
            case Bottom => BoolLattice[B].bottom
            case _ => BoolLattice[B].inject(e == Inexact)        

    def eql[B: BoolLattice](x: E, y: E): B = BoolLattice[B].inject(x == y)
                    

    override def subsumes(x: E, y: => E): Boolean =
        if x == Top then
            true
        else if x == Bottom then
            isBottom(y)
        else if x == Exact then
            y == Bottom || y == Exact
        else
            y == Bottom || y == Inexact

    def join(x: E, y: => E): E =
        x match
            case Top => Top
            case Bottom => y
            case Exact =>
                y match
                    case Exact => Exact
                    case Bottom => Exact
                    case _ => Top
            case Inexact =>
                y match
                    case Inexact => Inexact
                    case Bottom => Inexact
                    case _ => Top

    private def contaminate(n1: E, n2: E): E =
        (n1, n2) match
            case (Bottom, _) => Bottom
            case (_, Bottom) => Bottom
            case (Inexact, _) => Inexact
            case (_, Inexact) => Inexact
            case (Top, _) => Top
            case (_, Top) => Top
            case _ => Exact

    def plus(n1: E, n2: E): E =
        contaminate(n1, n2)

    def minus(n1: E, n2: E): E =
        contaminate(n1, n2)

    def times(n1: E, n2: E): E =
        contaminate(n1, n2)

    def quotient(n1: E, n2: E): E =
        contaminate(n1, n2)


    def exactToInexact(n: E): E = Inexact
    def inexactToExact(n: E): E = Exact
    def ceiling(n: E): E = n
    def floor(n: E): E = n
    def round(n: E): E = n
    def expt(n1: E, n2: E): E = contaminate(n1, n2)
    private def singularOp(n: E): E =
        n match
            case Exact => Top
            case _ => n

    def acos(n: E): E = singularOp(n)
    def atan(n: E): E = singularOp(n)
    def cos(n: E): E = singularOp(n)
    def asin(n: E): E = singularOp(n)
    def sin(n: E): E = singularOp(n)
    def tan(n: E): E = singularOp(n)
    def log(n: E): E = singularOp(n)
    def sqrt(n: E): E = singularOp(n)
    def modulo(n1: E, n2: E): E = contaminate(n1, n2)
    def div(n1: E, n2: E): E = Top /// nog aanpassen
    def remainder(n1: E, n2: E): E = contaminate(n1, n2)
}
