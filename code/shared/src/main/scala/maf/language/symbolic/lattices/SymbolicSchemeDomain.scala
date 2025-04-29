package maf.language.symbolic.lattices

import maf.language.symbolic.*
import maf.core.{Address, Lattice}
import maf.language.scheme.primitives._
import maf.lattice.*
import maf.language.scheme.lattices.*
import maf.modular.scheme.SchemeDomain

trait SymbolicSchemeDomain extends SchemeDomain:
    implicit lazy val lattice: Product2SchemeLattice[Value, SymbolicLattice.L, Address]

trait SymbolicSchemeConstantPropagationDomain extends SymbolicSchemeDomain:
    type S = ConstantPropagation.S
    type B = ConstantPropagation.B
    type C = ConstantPropagation.C
    type Sym = ConstantPropagation.Sym
    type N = ConstantPropagation.N

    given Lattice[SymbolicLattice.L] = new SymbolicLattice[B]

    type Value = modularLattice.PL
    final val modularLattice = new Product2ModularSchemeLattice[Address, S, B, C, Sym, N, SymbolicLattice.L]
    implicit lazy val lattice = modularLattice.product2Lattice
    final lazy val primitives = new SchemeLatticePrimitives()(modularLattice.product2Lattice)
