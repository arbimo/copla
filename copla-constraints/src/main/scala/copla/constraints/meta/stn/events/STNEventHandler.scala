package copla.constraints.meta.stn.events

import copla.constraints.meta.{CSP, CSPUpdateResult}
import copla.constraints.meta.constraints.Constraint
import copla.constraints.meta.events._
import copla.constraints.meta.stn.constraint.TemporalConstraint
import copla.constraints.meta.stn.core.IDistanceChangeListener
import copla.constraints.meta.stn.variables.{TemporalDelay, Timepoint}
import copla.constraints.meta.util.Assertion._
import copla.constraints.stnu.InconsistentTemporalNetwork

import scala.util.{Failure, Success, Try}

class STNEventHandler(implicit val csp: CSP)
    extends InternalCSPEventHandler
    with IDistanceChangeListener {

  def stn = csp.stn

  override def handleEvent(event: Event): CSPUpdateResult = {
    Try {
      event match {
        case NewVariableEvent(tp: Timepoint) =>
          // record any new timepoint in the STN, with special case for Origin and horizon
          if (tp == csp.temporalOrigin) {
            stn.recordTimePointAsStart(tp)
          } else if (tp == csp.temporalHorizon) {
            stn.recordTimePointAsEnd(tp)
          } else {
            stn.recordTimePoint(tp)
            if (csp.conf.enforceTpAfterStart)
              stn.enforceBefore(csp.temporalOrigin, tp)
            stn.enforceBefore(tp, csp.temporalHorizon)
          }
        case NewConstraint(c: TemporalConstraint) =>
        // nothing to do, handled in propagation directly
        case NewConstraint(c) =>
          // not a pure temporal constraint, check if there is any temporal variables to watch in it
          for ((tp1, tp2) <- watches(c))
            stn.addWatchedDistance(tp1, tp2)
        case WatchConstraint(c) =>
          for ((tp1, tp2) <- watches(c))
            stn.addWatchedDistance(tp1, tp2)
        case Satisfaction(c: TemporalConstraint) => // no watches were recorded by this constraint
        case Satisfaction(c)                     =>
          // not a pure temporal constraint, check if there is any temporal variables to watch in it
          for ((tp1, tp2) <- watches(c))
            stn.removeWatchedDistance(tp1, tp2)
        case UnwatchConstraint(c) =>
          for ((tp1, tp2) <- watches(c))
            stn.removeWatchedDistance(tp1, tp2)
        case _ =>
      }
      watchesSanityChecks()
    } match {
      case Failure(e: InconsistentTemporalNetwork) =>
        CSPUpdateResult.inconsistent("Inconsistent temporal network")
      case Failure(e) => CSPUpdateResult.fatal(e)
      case Success(_) => CSPUpdateResult.consistent
    }
  }

  /** Returns all delay that a gien constraint should be monitoring. */
  private def watches(c: Constraint): Iterable[(Timepoint, Timepoint)] = {
    c.variables.collect {
      case tp: Timepoint    => (csp.temporalOrigin, tp)
      case d: TemporalDelay => (d.from, d.to)
    }
  }

  /** Checks that all delay monitored by actived and watched constraints are notified to the STN*/
  private def watchesSanityChecks() {
    if (csp.events.isEmpty) { // there might be non recorded event watches as long as the event queue is not empty
      assert3(csp.constraints.active.flatMap(watches(_)).forall(p => stn.isWatched(p._1, p._2)),
              "A distance of an active constraint is not recorded in the STN")
      assert3(csp.constraints.watched.flatMap(watches(_)).forall(p => stn.isWatched(p._1, p._2)),
              "A distance of a watched constraint is not recorded in the STN")
    }
  }

  /** Handles the notification from the STN that the distance between two timepoints has been updated. */
  override def distanceUpdated(tp1: Timepoint, tp2: Timepoint) {
    csp.addEvent(DomainReduced(csp.varStore.getDelayVariable(tp1, tp2)))
    if (tp1 == csp.temporalOrigin)
      csp.addEvent(DomainReduced(tp2))
  }

  override def clone(newCSP: CSP): STNEventHandler = new STNEventHandler()(newCSP)
}
