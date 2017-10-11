package copla.lang

import copla.lang.model.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable

package object analysis {

  sealed trait AbstractionHierarchyType
  case object Knoblock extends AbstractionHierarchyType
  case object DetailedKnoblock extends AbstractionHierarchyType

  /** Tarjan's algorithm for extracting strongly connected components.
    *
    * Given a directed graph, the algorithm outputs a sequence of strongly connected
    * components sorted in topological order.
    * */
  private[analysis] def tarjan[V](graph: Map[V, Set[V]]): Seq[Set[V]] = {
    class Data(var index: Int, var lowLink: Int, var onStack: Boolean)
    var index = 0
    val stack     = new ArrayBuffer[V]()
    val data  = mutable.Map[V, Data]()

    var components: Seq[Set[V]] = immutable.Seq()

    for (v <- graph.keys) {
      if (!data.contains(v))
        strongConnect(v)
    }

    def strongConnect(v: V): Unit = {
      assert(!data.contains(v))
      data(v) = new Data(index, index, true)
      index += 1
      stack += v
      for (w <- graph.getOrElse(v, Set())) {
        if (!data.contains(w)) {
          strongConnect(w)
          data(v).lowLink = data(v).lowLink.min(data(w).lowLink)
        } else if (data(w).onStack) {
          data(v).lowLink = data(v).lowLink.min(data(w).index)
        }
      }

      if (data(v).lowLink == data(v).index) {
        var scc: Set[V] = Set()
        var w           = stack.last
        do {
          w = stack.last
          stack.remove(stack.size - 1)
          data(w).onStack = false
          scc += w
        } while (w != v)
        components = components :+ scc
      }
    }

    components.reverse
  }

  /** Returns a set of fluent templates on which the given action has an effect. */
  def affectedBy(a: ActionTemplate): Set[FluentTemplate] = {
    val extractor = landscaper.pattern {
      case x: TimedAssertion with ProvidesChange => Seq(x.fluent.template)
    }
    landscaper.extract(extractor, a).toSet
  }

  /** Returns a set of fluent templates on which the given action has a condition. */
  def conditionedBy(a: ActionTemplate): Set[FluentTemplate] = {
    val extractor = landscaper.pattern {
      case x: TimedAssertion with RequiresSupport => Seq(x.fluent.template)
    }
    landscaper.extract(extractor, a).toSet
  }

  /** Computes an abstraction hierarchy, has defined by Knoblock.
    * Each fluent is associated to a level such that a fluent at a level does not affect the fluents at the level N+1. */
  def abstractionHierarchy(model: CoreModel, typ: AbstractionHierarchyType): Map[FluentTemplate, Int] = {
    val graph: Map[FluentTemplate, mutable.Set[FluentTemplate]] =
      model.collect { case FunctionDeclaration(x: FluentTemplate) => (x, mutable.Set[FluentTemplate]()) }.toMap

    val actions = model.collect { case x: ActionTemplate => x }

    // extract the dependency graph between fluents.
    for(a <- actions) {
      val affected = affectedBy(a)
      val conditioned = conditionedBy(a)
      for(eff <- affected) {
        for(eff2 <- affected if eff2 != eff) {
          // eff and eff2 are both effects in a, marked them as interdependent
          graph(eff) += eff2
          graph(eff2) += eff
        }
        for(cond <- conditioned if cond != eff)
          // eff affects cond
          graph(eff) += cond
      }
    }

    val sccs = tarjan(graph.mapValues(_.toSet))
    val postProcessed = typ match {
      case Knoblock => sccs
      case DetailedKnoblock => sccs.flatMap(scc => subHierarchy(scc, actions))
    }

    postProcessed
      .zipWithIndex
      .flatMap({ case (scc, lvl) => scc.toSeq.map((_, lvl))})
      .toMap
  }

  def subHierarchy(fluents: Set[FluentTemplate], actions: Seq[ActionTemplate]): Seq[Set[FluentTemplate]] = {
    val graph: Map[FluentTemplate, mutable.Set[FluentTemplate]] =
      fluents.map(_ -> mutable.Set[FluentTemplate]()).toMap

    def affectsFirstButNotSecond(action: ActionTemplate, f1: FluentTemplate, f2: FluentTemplate): Boolean = {
      affectedBy(action).contains(f1) && !affectedBy(action).contains(f2)
    }
    def affectsBoth(action: ActionTemplate, f1: FluentTemplate, f2: FluentTemplate): Boolean = {
      affectedBy(action).contains(f1) && affectedBy(action).contains(f2)
    }

    for(f1 <- fluents ; f2 <- fluents if f1 != f2) {
      if (actions.exists(affectsFirstButNotSecond(_, f1, f2)))
        graph(f2) += f1

      if(actions.exists(affectsFirstButNotSecond(_, f2, f1)))
        graph(f1) += f2

      if(!actions.exists(affectsFirstButNotSecond(_, f2, f1)) && !actions.exists(affectsFirstButNotSecond(_, f1, f2))) {
        graph(f1) += f2
        graph(f2) += f1
      }

    }
    tarjan(graph.mapValues(_.toSet))
  }
}
