package maf.lattice

import maf.core.*
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, NotANumberString, NumberLattice, RealLattice, StringLattice, SymbolLattice}
import spire.math.Complex

object Type:
    sealed trait T:
        def to[L: Lattice]: L = this match
            case Bottom => Lattice[L].bottom
            case Top    => Lattice[L].top
    case object Top extends T
    case object Bottom extends T

    abstract class BaseInstance(typeName: String) extends Lattice[T]:
        def show(x: T): String = x match
            case Top    => typeName
            case Bottom => s"$typeName.⊥"
        val bottom: T = Bottom
        val top: T = Top
        def join(x: T, y: => T): T = x match
            case Top    => Top
            case Bottom => y
        def meet(x: T, y: => T): T = x match
            case Bottom => Bottom
            case Top    => y
        def subsumes(x: T, y: => T): Boolean = x match
            case Top => true
            case Bottom =>
                y match
                    case Top    => false
                    case Bottom => true
        def eql[B2: BoolLattice](n1: T, n2: T): B2 = (n1, n2) match
            case (Top, Top) => BoolLattice[B2].top
            case _          => BoolLattice[B2].bottom

    type S = T
    type B = T
    type I = T
    type R = T
    type C = T
    type Sym = T
    type N = T

    object T:

        implicit val typeIsString: StringLattice[S] = new BaseInstance("Str") with StringLattice[S] {
            def inject(x: String): T = Top
            def length[N2: NumberLattice](s: T) = s.to[N2]
            def append(s1: T, s2: T) = (s1, s2) match
                case (Bottom, _) | (_, Bottom) => Bottom
                case (Top, _) | (Top, _)       => Top
            def ref[N2: NumberLattice, C2: CharLattice](s: S, i: N2): C2 = s match
                case Bottom                            => CharLattice[C2].bottom
                case Top if NumberLattice[N2].isBottom(i) => CharLattice[C2].bottom
                case Top                               => CharLattice[C2].top
            def set[N2: NumberLattice, C2: CharLattice](
                s: S,
                i: N2,
                c: C2
              ): S =
                if s == Bottom || NumberLattice[N2].isBottom(i) || CharLattice[C2].isBottom(c) then Bottom
                else Top
            def lt[B2: BoolLattice](s1: T, s2: T) = (s1, s2) match
                case (Bottom, _) | (_, Bottom) => BoolLattice[B2].bottom
                case (Top, _) | (Top, _)       => BoolLattice[B2].top
            def substring[N2: NumberLattice](
                s: T,
                from: N2,
                to: N2
              ): T = s match
                case Bottom                             => Bottom
                case _ if NumberLattice[N2].isBottom(from) => Bottom
                case _ if NumberLattice[N2].isBottom(to)   => Bottom
                case Top                                => Top

            def toSymbol[Sym2: SymbolLattice](s: S) = s.to[Sym2]
            def toNumber[N2: NumberLattice](s: S) = s match
                case Bottom => MayFail.success(NumberLattice[N2].bottom)
                case Top    => MayFail.success(NumberLattice[N2].top).addError(NotANumberString)
        }
        implicit val typeIsBoolean: BoolLattice[B] = new BaseInstance("Bool") with BoolLattice[B] {
            def inject(x: Boolean): T = Top
            def isTrue(b: T) = b == Top
            def isFalse(b: T) = b == Top
            def not(b: T) = b
        }
        implicit val typeIsInteger: IntLattice[I] = new BaseInstance("Int") with IntLattice[I] {
            def inject(x: BigInt): T = Top
            def toReal[R2: RealLattice](n: T): R2 = n.to[R2]
            def toComplex[Comp2: NumberLattice](n: T): Comp2 = n.to[Comp2]
            def random(n: T): T = n
            def plus(n1: T, n2: T): T = meet(n1, n2)
            def minus(n1: T, n2: T): T = meet(n1, n2)
            def times(n1: T, n2: T): T = meet(n1, n2)
            def div[R2: RealLattice](n1: T, n2: T): R2 = (n1, n2) match
                case (Top, Top) => RealLattice[R2].top
                case _          => RealLattice[R2].bottom
            def expt(n1: T, n2: T): T = meet(n1, n2)
            def quotient(n1: T, n2: T): T = meet(n1, n2)
            def modulo(n1: T, n2: T): T = meet(n1, n2)
            def remainder(n1: T, n2: T): T = meet(n1, n2)
            def lt[B2: BoolLattice](n1: T, n2: T): B2 = (n1, n2) match
                case (Top, Top) => BoolLattice[B2].top
                case _          => BoolLattice[B2].bottom
            def valuesBetween(n1: T, n2: T): Set[T] = Set(Top)
            def makeString[C2: CharLattice, S2: StringLattice](length: I, char: C2): S2 = StringLattice[S2].top
            def toString[S2: StringLattice](n: T): S2 = n.to[S2]
            def toChar[C2: CharLattice](n: T): C2 = n.to[C2]
        }
        implicit val typeIsReal: RealLattice[R] = new BaseInstance("Real") with RealLattice[R] {
            def inject(x: Double): T = Top
            def toInt[I2: IntLattice](n: T): I2 = n.to[I2]
            def toComplex[Comp2: NumberLattice](n: T): Comp2 = n.to[Comp2]
            def ceiling(n: T): T = n
            def floor(n: T): T = n
            def round(n: T): T = n
            def log(n: T): T = n
            def random(n: T): T = n
            def sin(n: T): T = n
            def asin(n: T): T = n
            def cos(n: T): T = n
            def acos(n: T): T = n
            def tan(n: T): T = n
            def atan(n: T): T = n
            def sqrt(n: T): T = n
            def plus(n1: T, n2: T): T = meet(n1, n2)
            def minus(n1: T, n2: T): T = meet(n1, n2)
            def times(n1: T, n2: T): T = meet(n1, n2)
            def div(n1: T, n2: T): T = meet(n1, n2)
            def expt(n1: T, n2: T): T = meet(n1, n2)
            def lt[B2: BoolLattice](n1: T, n2: T): B2 = (n1, n2) match
                case (Top, Top) => BoolLattice[B2].top
                case _          => BoolLattice[B2].bottom
            def toString[S2: StringLattice](n: T): S2 = n.to[S2]
        }

        implicit val typeIsComplex: NumberLattice[N] = new BaseInstance("Complex") with NumberLattice[N] {
            def inject(x: Double): T = Top
            def inject(x: Complex[Double]): T = Top
            def inject(x: BigInt): T = Top

            case object Real extends T
            case object Integer extends T
            case object ExclReal extends T
            case object ExclComplex extends T
            val real = Real
            val integer = Integer
            val exclReal = ExclReal
            val exclComplex = ExclComplex
            
            def log(n: T): T = n
            def random(n: T): T = n
            def sin(n: T): T = n
            def asin(n: T): T = n
            def cos(n: T): T = n
            def acos(n: T): T = n
            def tan(n: T): T = n
            def atan(n: T): T = n
            def sqrt(n: T): T = n
            def plus(n1: T, n2: T): T = meet(n1, n2)
            def minus(n1: T, n2: T): T = meet(n1, n2)
            def times(n1: T, n2: T): T = meet(n1, n2)
            def div(n1: T, n2: T): T = meet(n1, n2)
            def expt(n1: T, n2: T): T = meet(n1, n2)
            def ceiling(n: T): T = n
            def floor(n: T): T = n
            def isComplex[B2: BoolLattice](n: T): B2 = n.to[B2]
            def round(n: T): T = n
            def isInteger[B2: BoolLattice](n: T): B2 = n.to[B2]
            def isReal[B2: BoolLattice](n: T): B2 = n.to[B2]
            def modulo(n1: T, n2: T): T = meet(n1, n2)
            def remainder(n1: T, n2: T): T = meet(n1, n2)
            def quotient(n1: T, n2: T): T = meet(n1, n2)
            def lt[B2: BoolLattice](n1: T, n2: T): B2 = (n1, n2) match
                case (Top, Top) => BoolLattice[B2].top
                case _ => BoolLattice[B2].bottom
            def toString[S2: StringLattice](n: T): S2 = n.to[S2]

            override def imagPart(n: N): N = ???
            override def toChar[C: CharLattice](n: N): C = ???
            override def realPart(n: N): N = ???
            override def makeString[C: CharLattice, S: StringLattice](length: N, char: C): S = ???
        }
        implicit val typeIsChar: CharLattice[C] = new BaseInstance("Char") with CharLattice[C] {
            def inject(c: Char): T = Top
            def downCase(c: C): C = c
            def upCase(c: C): C = c
            def toString[S2: StringLattice](c: C): S2 = c.to[S2]
            def toInt[N2: NumberLattice](c: C): N2 = c.to[N2]
            def isLower[B2: BoolLattice](c: C): B2 = c.to[B2]
            def isUpper[B2: BoolLattice](c: C): B2 = c.to[B2]

            override def charEq[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Top, Top) => BoolLattice[B2].top
                case _          => BoolLattice[B2].bottom
            override def charLt[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Top, Top) => BoolLattice[B2].top
                case _          => BoolLattice[B2].bottom
            def charEqCI[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Top, Top) => BoolLattice[B2].top
                case _          => BoolLattice[B2].bottom
            def charLtCI[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Top, Top) => BoolLattice[B2].top
                case _          => BoolLattice[B2].bottom
        }
        implicit val typeIsSymbol: SymbolLattice[Sym] = new BaseInstance("Sym") with SymbolLattice[Sym] {
            def inject(sym: String): T = Top
            def toString[S2: StringLattice](s: T): S2 = StringLattice[S2].top
        }
