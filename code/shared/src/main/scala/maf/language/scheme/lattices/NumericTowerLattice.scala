package maf.language.scheme.lattices

import maf.lattice.{AbstractWrapType, interfaces}
import maf.lattice.interfaces.{BoolLattice, IntLattice, NumberLattice, RealLattice}
import spire.math.Complex

class NumericTowerLattice[I: IntLattice, R: RealLattice, Comp: NumberLattice] extends NumberLattice[(I, R, Comp)] {

    private sealed trait NumType

    private case class NInt(i: I) extends NumType
    private case class NReal(r: R) extends NumType
    private case class NComp(c: Comp) extends NumType

    private def wrap(n: (I, R, Comp)): (NumType, NumType, NumType) = (NInt(n._1), NReal(n._2), NComp(n._3))

    def getInt(n: (I, R, Comp)): I = n._1
    def getReal(n: (I, R, Comp)): R = RealLattice[R].join(n._2, IntLattice[I].toReal[R](n._1))
    def getComp(n: (I, R, Comp)): Comp = n._3

    override def show(n: (I, R, Comp)): String =
        "(" + IntLattice[I].show(n._1) +  ", " + RealLattice[R].show(n._2) + ", " + NumberLattice[Comp].show(n._3) + ")"
        
    override def inject(n: Double): (I, R, Comp) =
        if n == 0 then
            inject(BigInt(0))
        else
            (IntLattice[I].bottom, RealLattice[R].inject(n), NumberLattice[Comp].bottom)
    
   // override def inject(n: Double): (I, R, Comp) = (IntLattice[I].bottom, RealLattice[R].inject(n))
    override def inject(n: BigInt): (I, R, Comp) = (IntLattice[I].inject(n), RealLattice[R].bottom, NumberLattice[Comp].bottom)
    override def inject(n: Complex[Double]): (I, R, Comp) = (IntLattice[I].bottom, RealLattice[R].bottom, NumberLattice[Comp].inject(n))

    override def top: (I, R, Comp) = (IntLattice[I].top, RealLattice[R].top, NumberLattice[Comp].top)
    override def bottom: (I, R, Comp) = (IntLattice[I].bottom, RealLattice[R].bottom, NumberLattice[Comp].bottom)

    private def intBottom: I = IntLattice[I].bottom
    private def realBottom: R = RealLattice[R].bottom

    private def intTop: I = IntLattice[I].top
    private def realTop: R = RealLattice[R].top

    private def castToReal(i: I): R = IntLattice[I].toReal[R](i)
    private def castToReal(n: NumType): R =
        n match
            case NInt(i) => castToReal(i)
            case NReal(r) => r
    private def castToComplex(n: NumType): Comp =
        n match
            case NInt(i) => IntLattice[I].toComplex[Comp](i)
            case NReal(r) => RealLattice[R].toComplex[Comp](r)
            case NComp(c) => c

    override def subsumes(x: (I, R, Comp), y: => (I, R, Comp)): Boolean =
        //(IntLattice[I].subsumes(getInt(x), getInt(y)) || RealLattice[R].subsumes(x._2, castToReal(getInt(y))))
        IntLattice[I].subsumes(getInt(x), getInt(y)) &&  RealLattice[R].subsumes(getReal(x), getReal(y))
    
    override def join(x: (I, R, Comp), y: => (I, R, Comp)): (I, R, Comp) =
        (IntLattice[I].join(x._1, y._1), RealLattice[R].join(x._2, y._2), NumberLattice[Comp].join(getComp(x), getComp(y)))

    private def foldMap(f: (NumType, NumType) => NumType, n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) =
        var intRes = intBottom
        var realRes = realBottom
        var compRes = NumberLattice[Comp].bottom
        for (e1 <- wrap(n1).productIterator) {
            for (e2 <- wrap(n2).productIterator) {
                (e1, e2) match
                    case (i1: NumType, i2: NumType) => f(i1, i2) match
                        case NInt(i) => intRes = IntLattice[I].join(intRes, i)
                        case NReal(r) => realRes = RealLattice[R].join(realRes, r)
                        case NComp(c) => compRes = NumberLattice[Comp].join(compRes, c)
            }
        }
        (intRes, realRes, compRes)

    private def foldMap(f: NumType => NumType, n: (I, R, Comp)): (I, R, Comp) =
        var intRes = intBottom
        var realRes = realBottom
        var compRes = NumberLattice[Comp].bottom
        for (e <- wrap(n).productIterator) {
            e match
                case i: NumType => f(i) match
                    case NInt(i) => intRes = IntLattice[I].join(intRes, i)
                    case NReal(r) => realRes = RealLattice[R].join(realRes, r)
                    case NComp(c) => compRes = NumberLattice[Comp].join(compRes, c)
        }
        (intRes, realRes, compRes)

    private def contaminate(n1: (I, R, Comp), n2: (I, R, Comp), fInt: (I, I) => I, fReal: (R, R) => R, fComp: (Comp, Comp) => Comp): (I, R, Comp) =
        foldMap(
            (e1, e2) => {
                (e1, e2) match
                    case (NInt(i1), NInt(i2)) => NInt(fInt(i1, i2))
                    case (NInt(i1), NReal(i2)) => NReal(fReal(castToReal(i1), i2))
                    case (NReal(i1), NInt(i2)) => NReal(fReal(i1, castToReal(i2)))
                    case (NReal(i1), NReal(i2)) => NReal(fReal(i1, i2))
                    case (NComp(i1), NInt(i2)) => NComp(fComp(i1, castToComplex(NInt(i2))))
                    case (NComp(i1), NReal(i2)) => NComp(fComp(i1, castToComplex(NReal(i2))))
                    case (NComp(i1), NComp(i2)) => NComp(fComp(i1, i2))
                    case (NInt(i1), NComp(i2)) => NComp(fComp(castToComplex(NInt(i1)), i2))
                    case (NReal(i1), NComp(i2)) => NComp(fComp(castToComplex(NReal(i1)), i2))
            },
            n1,
            n2
        )

    override def plus(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = contaminate(n1, n2, IntLattice[I].plus, RealLattice[R].plus, NumberLattice[Comp].plus)
    override def minus(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = contaminate(n1, n2, IntLattice[I].minus, RealLattice[R].minus, NumberLattice[Comp].minus)
    override def times(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = contaminate(n1, n2, IntLattice[I].times, RealLattice[R].times, NumberLattice[Comp].times)
    override def expt(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = contaminate(n1, n2, IntLattice[I].expt, RealLattice[R].expt, NumberLattice[Comp].expt)

    private def compare[B: BoolLattice](n1: (I, R, Comp), n2: (I, R, Comp), fCompare: (R, R) => B): B =
        var res: B = BoolLattice[B].bottom
        for (e1 <- wrap(n1).productIterator) {
            for (e2 <- wrap(n2).productIterator) {
                (e1, e2) match
                    case (i1 : NumType, i2: NumType) => res = BoolLattice[B].join(res, fCompare(castToReal(i1), castToReal(i2)))
            }
        }
        res

    override def lt[B: BoolLattice](n1: (I, R, Comp), n2: (I, R, Comp)): B = compare[B](n1, n2, RealLattice[R].lt)
    override def eql[B: BoolLattice](n1: (I, R, Comp), n2: (I, R, Comp)): B = compare[B](n1, n2, RealLattice[R].eql)

    override def div(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) =
        foldMap(
            (e1: NumType, e2: NumType) => {
                def res = RealLattice[R].div(castToReal(e1), castToReal(e2))
                if RealLattice[R].round(res) == res then
                    NInt(RealLattice[R].toInt[I](res))
                else
                    NReal(res)},
            n1, n2
        )

    //p.23 R5RS spec
    private def generalRound(n: (I, R, Comp), fRound: R => R): (I, R, Comp) =
        foldMap((elem: NumType) => {
            elem match
                case i: NInt => i
                case r: NReal => NInt(RealLattice[R].toInt[I](fRound(castToReal(elem))))
                case c: NComp => NComp(NumberLattice[Comp].bottom)
        },
            n)
    
    override def ceiling(n: (I, R, Comp)): (I, R, Comp) = generalRound(n, RealLattice[R].ceiling)
    override def floor(n: (I, R, Comp)): (I, R, Comp) = generalRound(n, RealLattice[R].floor)
    override def round(n: (I, R, Comp)): (I, R, Comp) = generalRound(n, RealLattice[R].round)

    //////////////////////////////////////////////////////////////////
    private def ToR(n: (I, R, Comp), fReal: R => R, fComp: Comp => Comp): (I, R, Comp) =
        def res: R = fReal(IntLattice[I].toReal[R](n._1))
        if res ==  RealLattice[R].round(res) then
            (RealLattice[R].toInt[I](res), fReal(n._2), fComp(n._3))
        else
            (intBottom, RealLattice[R].join(fReal(n._2), res), fComp(n._3))

    override def acos(n: (I, R, Comp)): (I, R, Comp) = ToR(n, RealLattice[R].acos, NumberLattice[Comp].acos)
    override def sqrt(n: (I, R, Comp)): (I, R, Comp) = ToR(n, RealLattice[R].sqrt, NumberLattice[Comp].sqrt)
    override def cos(n: (I, R, Comp)): (I, R, Comp) = ToR(n, RealLattice[R].cos, NumberLattice[Comp].cos)
    override def atan(n: (I, R, Comp)): (I, R, Comp) = ToR(n, RealLattice[R].atan, NumberLattice[Comp].atan)
    override def sin(n: (I, R, Comp)): (I, R, Comp) = ToR(n, RealLattice[R].sin, NumberLattice[Comp].sin)
    override def tan(n: (I, R, Comp)): (I, R, Comp) = ToR(n, RealLattice[R].tan, NumberLattice[Comp].tan)
    override def asin(n: (I, R, Comp)): (I, R, Comp) = ToR(n, RealLattice[R].asin, NumberLattice[Comp].asin)

    private def MAndR(n1: (I, R, Comp), n2: (I, R, Comp), f: (I, I) => I) : (I, R, Comp) =
        (f(n1._1, n2._1), realBottom, NumberLattice[Comp].bottom)

    override def remainder(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = MAndR(n1, n2, IntLattice[I].remainder)
    override def modulo(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = MAndR(n1, n2, IntLattice[I].modulo)
    override def quotient(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = MAndR(n1, n2, IntLattice[I].quotient)

    override def toInt(n: (I, R, Comp)): (I, R, Comp) =
        (IntLattice[I].join(
                n._1,
                RealLattice[R].toInt[I](n._2)), 
            realBottom, 
            NumberLattice[Comp].bottom)

    override def toReal(n: (I, R, Comp)): (I, R, Comp) = (intBottom, RealLattice[R].join(castToReal(n._1), n._2), NumberLattice[Comp].bottom)

    override def exactToInexact(n: (I, R, Comp)): (I, R, Comp) = n
    override def inexactToExact(n: (I, R, Comp)): (I, R, Comp) = n

    override def log(n: (I, R, Comp)): (I, R, Comp) = foldMap(elem => NReal(RealLattice[R].log(castToReal(elem))), n)
}