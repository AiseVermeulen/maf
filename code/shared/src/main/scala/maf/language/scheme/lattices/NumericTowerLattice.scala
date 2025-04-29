/*
package maf.language.scheme.lattices

import maf.lattice.{AbstractWrapType, interfaces}
import maf.lattice.interfaces.{BoolLattice, IntLattice, NumberLattice, RealLattice}
import spire.math.Complex

class NumericTowerLattice[I: IntLattice, R: RealLattice, Comp: NumberLattice] extends NumberLattice[Comp] {
        
    override def inject(n: Double): Comp = NumberLattice[Comp].inject(n)
    override def inject(n: BigInt): Comp = NumberLattice[Comp].inject(n)
    override def inject(n: Complex[Double]): Comp = NumberLattice[Comp].inject(n)

    override def plus(n1: Comp, n2: Comp): Comp = ???
    

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

    private def compare[B: BoolLattice](n1: (I, R, Comp), n2: (I, R, Comp), fCompare: (R, R) => B): B =
        var res: B = BoolLattice[B].bottom
        for (e1 <- wrap(n1).productIterator) {
            for (e2 <- wrap(n2).productIterator) {
                (e1, e2) match
                    case (i: NComp, _) => BoolLattice[B].bottom
                    case (_, i: NComp) => BoolLattice[B].bottom
                    case (i1 : NumType, i2: NumType) => res = BoolLattice[B].join(res, fCompare(castToReal(i1), castToReal(i2)))
            }
        }
        res

    def lt[B: BoolLattice](n1: (I, R, Comp), n2: (I, R, Comp)): B = compare[B](n1, n2, RealLattice[R].lt)
    override def eql[B: BoolLattice](n1: (I, R, Comp), n2: (I, R, Comp)): B = compare[B](n1, n2, RealLattice[R].eql) //

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
    
    def ceiling(n: (I, R, Comp)): (I, R, Comp) = generalRound(n, RealLattice[R].ceiling)
    def floor(n: (I, R, Comp)): (I, R, Comp) = generalRound(n, RealLattice[R].floor)
    def round(n: (I, R, Comp)): (I, R, Comp) = generalRound(n, RealLattice[R].round)

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

    def remainder(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = MAndR(n1, n2, IntLattice[I].remainder)
    def modulo(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = MAndR(n1, n2, IntLattice[I].modulo)
    def quotient(n1: (I, R, Comp), n2: (I, R, Comp)): (I, R, Comp) = MAndR(n1, n2, IntLattice[I].quotient)

    def toInt(n: (I, R, Comp)): (I, R, Comp) =
        (IntLattice[I].join(
                n._1,
                RealLattice[R].toInt[I](n._2)), 
            realBottom, 
            NumberLattice[Comp].bottom)

    def toReal(n: (I, R, Comp)): (I, R, Comp) = (intBottom, RealLattice[R].join(castToReal(n._1), n._2), NumberLattice[Comp].bottom)

    override def log(n: (I, R, Comp)): (I, R, Comp) = foldMap(
        elem =>
            elem match
                case NInt(i) => NReal(RealLattice[R].log(castToReal(i)))
                case NReal(r) => NReal(RealLattice[R].log(r))
                case NComp(c) => NComp(NumberLattice[Comp].log(c)),
        n)
}

 */