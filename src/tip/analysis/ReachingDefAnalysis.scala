package tip.analysis

import tip.ast._
import tip.cfg._
import tip.cfg.CfgOps._
import tip.ast.AstOps._
import tip.lattices.{MapLattice, PowersetLattice}
import tip.solvers.{SimpleMapLatticeFixpointSolver, SimpleWorklistFixpointSolver}
import tip.ast.AstNodeData.DeclarationData

/**
  * Base class for available expressions analysis
  */
abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

    val allAssigns: Set[AAssignStmt] = cfg.nodes.flatMap(_.appearingAssignments)

    NoPointers.assertContainsProgram(cfg.prog)
    NoRecords.assertContainsProgram(cfg.prog)

    val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allAssigns))

    def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
      n match {
        case _: CfgFunEntryNode => lattice.sublattice.bottom
        case r: CfgStmtNode =>
          r.data match {
              case assign: AAssignStmt => 
                (s.filter {
                  e =>
                    e.left match {
                      case id: AIdentifier =>
                        !(assign.left.appearingIds contains declData(id))
                      case _ => ??? //assign through pointer not implemented
                    }
                }) + assign
              case _ => s 
          }
        case _ => s
      }
}

/**
  * Available expressions analysis that uses the simple fipoint solver.
  */
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
  * Available expressions analysis that uses the worklist solver.
  */
class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies