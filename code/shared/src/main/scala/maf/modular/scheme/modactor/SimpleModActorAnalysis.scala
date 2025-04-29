package maf.modular.scheme.modactor

import maf.language.AScheme.lattices.*
import maf.language.scheme.SchemeExp
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.core.worklist.FIFOWorkList
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.modular.scheme.modconc.StandardSchemeModConcAllocator
import maf.language.AScheme.ASchemeValues.Behavior
import maf.modular.scheme.modf.StandardSchemeModFAllocator
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.worklist.RandomWorklistAlgorithm
import maf.core.Address
import maf.lattice.HMap
import maf.language.scheme.lattices.SchemeLattice
import maf.language.AScheme.ASchemeLattice
import maf.modular.scheme.SchemeDomain
import maf.lattice.ConstantPropagation
import maf.language.scheme.primitives.SchemeLatticePrimitives
import maf.language.racket.RacketLoaderSemantics

trait ASchemeConstantPropagationDomain extends SchemeDomain:
    type S = ConstantPropagation.S
    type B = ConstantPropagation.B
    type C = ConstantPropagation.C
    type Sym = ConstantPropagation.Sym
    type N = ConstantPropagation.N
    type Value = HMap

    implicit override lazy val lattice: ASchemeModularLattice[Address, S, B, C, Sym, N] =
        new ASchemeModularLattice[Address, S, B, C, Sym, N]()

    final lazy val primitives = new SchemeLatticePrimitives()(lattice)

class SimpleSchemeModActorAnalysis(program: SchemeExp)
    extends SimpleGlobalStoreModActor(program)
    with ASchemeConstantPropagationDomain
    with PowersetMailboxAnalysis
    with RacketLoaderSemantics
//with SchemeModActorFutures
