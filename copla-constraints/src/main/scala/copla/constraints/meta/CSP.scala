package copla.constraints.meta

import copla.constraints.meta.constraints._
import copla.constraints.meta.decisions.DecisionsHandler
import copla.constraints.meta.domains.{BooleanDomain, Domain, IntervalDomain}
import copla.constraints.meta.events._
import copla.constraints.meta.logger.{ILogger, Logger}
import copla.constraints.meta.search.heuristics.Heuristic
import copla.constraints.meta.stn.core.StnWithStructurals
import copla.constraints.meta.stn.events.STNEventHandler
import copla.constraints.meta.stn.variables.{TemporalDelay, Timepoint}
import copla.constraints.meta.types.TypesStore
import copla.constraints.meta.types.events.NewInstance
import copla.constraints.meta.types.statics.TypedVariable
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.{VariableStore, _}
import slogging.{LazyLogging, StrictLogging}

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import updates._
import cats.implicits._
import copla.constraints.meta.constraints.specialization.Specialization
import copla.constraints.meta.handlers.DomainsStore

class CSP(toClone: Either[Configuration, CSP] = Left(new Configuration))
    extends Ordered[CSP]
    with CSPView
    with StrictLogging {
  implicit private val csp = this

  val conf: Configuration = toClone match {
    case Left(configuration) => configuration
    case Right(base)         => base.conf
  }

  val depth: Int = toClone match {
    case Left(configuration) => configuration.initialDepth
    case Right(base)         => base.depth + 1
  }

  /** Number of CSPs that have been cloned from this one, updated by the child on its creation. */
  private[meta] var numberOfChildren = 0

  /** Indicates the rank inside the children of this CSP's parent. E.g. 0 indicate that it was first child/clone */
  private[meta] val placeInChildrenOfParent = toClone match {
    case Left(configuration) => 0
    case Right(base) =>
      base.numberOfChildren += 1
      base.numberOfChildren
  }

  val events: mutable.Queue[Event] = toClone match {
    case Right(base) => base.events.clone()
    case _           => mutable.Queue()
  }

  val eventHandlers: mutable.ArrayBuffer[InternalCSPEventHandler] = toClone match {
    case Right(base) => base.eventHandlers.map(handler => handler.clone(this))
    case Left(configuration) =>
      mutable.ArrayBuffer(new TypesStore(this),
                          new DecisionsHandler(this),
                          configuration.initialHeuristicBuilder(this),
                          new DomainsStore(this))
  }

  val types: TypesStore = getHandler(classOf[TypesStore])

  val decisions: DecisionsHandler = getHandler(classOf[DecisionsHandler])

  val domains: DomainsStore = getHandler(classOf[DomainsStore])

  val heuristic: Heuristic = eventHandlers.toList.collect { case x: Heuristic => x } match {
    case h :: Nil => h
    case _ =>
      throw new RuntimeException("Too many or not enough heuristics in the event handlers.")
  }

  val varStore: VariableStore = toClone match {
    case Right(base) => base.varStore.clone(this)
    case _           => new VariableStore(this)
  }

  val constraints: ConstraintStore = toClone match {
    case Right(base) => base.constraints.clone(this)
    case _           => new ConstraintStore(this)
  }

  val stn: StnWithStructurals = toClone match {
    case Right(base) => base.stn.clone()
    case _           => new StnWithStructurals()
  }
  val stnBridge: STNEventHandler = toClone match {
    case Right(base) => base.stnBridge.clone(this)
    case _           => new STNEventHandler()(this)
  }
  // set the STN listener to the one already in the event handlers to get notified of temporal variable updates
  stn.setDistanceChangeListener(stnBridge)

  val temporalOrigin  = varStore.getTimepoint(":start:")
  val temporalHorizon = varStore.getTimepoint(":end:")

  override def clone: CSP = new CSP(Right(this))

  def addHandler(handler: InternalCSPEventHandler) {
    eventHandlers += handler
  }

  override def getHandler[T](clazz: Class[T]): T = {
    eventHandlers.filter(_.getClass == clazz).toList match {
      case Nil      => throw new IllegalArgumentException("No handler of such type")
      case h :: Nil => h.asInstanceOf[T]
      case list     => throw new IllegalArgumentException("Multiple handlers of such type")
    }
  }

  /** Denotes the default domain of temporal variables. */
  private val defaultTimepointDomain = new IntervalDomain(Int.MinValue / 2, Int.MaxValue / 2)

  def dom(tp: Timepoint): IntervalDomain =
    try {
      new IntervalDomain(stn.getEarliestTime(tp), stn.getLatestTime(tp))
    } catch {
      case NonFatal(e: RuntimeException) =>
        defaultTimepointDomain
    }

  def dom(d: TemporalDelay): IntervalDomain =
    try {
      val min = stn.getMinDelay(d.from.tp, d.to.tp) + d.to.delay - d.from.delay
      val max = stn.getMaxDelay(d.from.tp, d.to.tp) + d.to.delay - d.from.delay
      new IntervalDomain(min, max)
    } catch {
      case NonFatal(e: RuntimeException) =>
        defaultTimepointDomain
    }

  def dom(v: IntVariable): Domain = domains.domOpt(v).getOrElse(v.initialDomain)

  def updateDomain(v: IntVariable, newDomain: Domain): Update =
    domains
      .updateDomain(v, newDomain)
      .flatMap(
        foreach(_)(addEvent(_))
      )

  def bind(variable: IntVariable, value: Int) {
    post(variable === value)
  }

  def propagate(): Update = {
    var result: Update = consistent

    while (result.ok && events.nonEmpty) {
      val e = events.dequeue()
      result = handleEvent(e)
    }
    result >>
      sanityCheck()
  }

  def sanityCheck(): Update = {
    check {
      assert1(events.isEmpty, "Can't sanity check: CSP has pending events")
      check3(
        constraints.active.forall(c => !c.isSatisfied && !c.isViolated),
        "Satisfaction of an active constraint is not UNDEFINED:\n " +
          constraints.active
            .collect {
              case c if c.isSatisfied || c.isViolated => c + " : " + c.satisfaction
            }
            .mkString("\n")
      )
      assert3(constraints.satisfied.forall(_.eventuallySatisfied),
              "A constraint is not satisfied while in the satisfied list")

      if (isSolution) {
        assert1(constraints.active.isEmpty)
        assert2(constraints.watched.isEmpty)
        assert3(stn.watchedVarsByIndex.values.map(_.size).sum == 0,
                stn.watchedVarsByIndex.values.flatten.toString())
      }
    }
  }

  private def propagate(constraint: Constraint, event: Event): Update = {
    def applyChange(change: OnPropagationChange): Update = {
      change match {
        case UpdateDomain(v, d) =>
          updateDomain(v, d)
        case Post(subConstraint) =>
          postSubConstraint(subConstraint, constraint)
        case Watch(sub) =>
          addWatcher(sub, constraint)
        case UpdateData(c, update) =>
          check { update(c.data) }
        case RetractDecision(decision) =>
          check { decisions.retract(decision) }
      }
    }
    val propagationResult = constraint.propagate(event)
    logger.debug(s"Propagation of $constraint ==> $propagationResult")
    propagationResult match {
      case Satisfied(changes) =>
        foreach(changes)(applyChange) >>
          setSatisfied(constraint) <<
          assert3(constraint.eventuallySatisfied)
      case Undefined(changes) =>
        foreach(changes)(applyChange)
      case Inconsistency =>
        assert3(
          constraint.eventuallyViolated,
          s"Constraint $constraint returned inconsistent while not eventually violated but ${constraint.satisfaction}.")
        inconsistent(s"Inconsistency detected during propagation of $constraint")
    }
  }

  def handleEvent(event: Event): Update = {
    logger.debug(s"Handling event: $event")
    constraints.handleEventFirst(event)

    val mainLoopResult: Update = event match {
      case NewConstraint(c) =>
        assert1(c.active)
        for (v <- c.variables) v match {
          case v: IntVariable if !domains.recorded(v) => addVariable(v)
          case _                                      =>
        }
        foreach(c.onPost) {
          case Watch(subConstraint)           => addWatcher(subConstraint, c)
          case Post(subConstraint)            => postSubConstraint(subConstraint, c)
          case DelegateToStn(tc)              => stn.addConstraint(tc)
          case InitData(constraint, data)     => constraints.setDataOf(constraint, data)
          case UpdateData(constraint, update) => check { update(constraint.data) }
          case AddDecision(dec)               => decisions.add(dec); consistent
        } >>
          propagate(c, event)

      case e: DomainChange =>
        foreach(constraints.watching(e.variable).filter(_.active))(c => {
          assert2(c.active)
          propagate(c, e)
        }) >>
          foreach(constraints.watching(e.variable).filter(_.watched))(c => {
            assert2(c.watched)
            if (c.eventuallySatisfied)
              addEvent(WatchedSatisfied(c))
            else if (c.eventuallyViolated)
              addEvent(WatchedViolated(c))
            else
              consistent
          })

      case event: WatchedSatisfactionUpdate =>
        // propagate all active constraints monitoring it
        foreach(
          constraints
            .monitoring(event.constraint))(c => {
          val propagationRes =
            if (c.active) propagate(c, event)
            else consistent

          // if it is watch, signal whether it is
          if (c.watched) {
            if (c.eventuallySatisfied)
              addEvent(WatchedSatisfied(c))
            else if (c.eventuallyViolated)
              addEvent(WatchedViolated(c))
          }
          propagationRes
        })

      case NewVariableEvent(v) =>
        foreach(v.unaryConstraints)(post)

      case Satisfaction(c) =>
        if (c.watched)
          addEvent(WatchedSatisfied(c))
        else
          consistent
      // handled by constraint store
      case WatchConstraint(c) => // already watched
        val onwatchRes = foreach(c.onWatch) {
          case Watch(sub) => addWatcher(sub, c)
        }
        onwatchRes >> {
          if (c.watched && c.eventuallySatisfied)
            addEvent(WatchedSatisfied(c))
          else if (c.watched && c.eventuallyViolated)
            addEvent(WatchedViolated(c))
          else
            consistent
        }


      case UnwatchConstraint(_) => consistent // handled by constraint store
      case WatchDelay(_, _)     => consistent // handled by STN
      case UnwatchDelay(_, _)   => consistent // handled by STN
      case _: NewInstance[_]    => consistent
      case e: CSPEvent          => fatal(s"CSPEvent $e was not properly handled")
      case _                    => consistent // not an internal CSP event, ignore
    }
    val result =
      mainLoopResult >>
        stnBridge.handleEvent(event) >>
        foreach(eventHandlers)(h => h.handleEvent(event)) <<
        constraints.handleEventLast(event)
    result
  }

  def post(constraint: Constraint): Update = {
    val s = Specialization(constraint)
    addEvent(NewConstraint(s))
  }

  def postSubConstraint(constraint: Constraint, parent: Constraint): Update = {
    post(constraint) // TODO, record relationship
  }

  def addWatcher(watched: Constraint, watching: Constraint): Update = {
    val specialized = Specialization(watched)
    constraints.addWatcher(specialized, watching)
  }

  def reified(constraint: Constraint): ReificationVariable = {
    if (!varStore.hasVariableForRef(constraint)) {
      val variable = varStore.getReificationVariable(constraint)
      domains.setDomain(variable, new BooleanDomain(Set(false, true)))
      post(new ReificationConstraint(variable, constraint))
    }
    varStore.getReificationVariable(constraint)
  }

  def setSatisfied(constraint: Constraint): Update = {
    assert2(constraint.eventuallySatisfied,
            s"Constraint $constraint is not (eventually) satisfied but ${constraint.satisfaction}")
    addEvent(Satisfaction(constraint))
  }

  def variable(ref: Any, dom: Set[Int]): IntVariable = {
    val v = new IntVar(Domain(dom), Some(ref))
    addVariable(v)
    v
  }

  def variable(ref: Any, lb: Int, ub: Int): IntVariable = {
    val v = new IntVar(new IntervalDomain(lb, ub), Some(ref))
    addVariable(v)
    v
  }

  def addVariable(variable: IntVariable): Update = {
    assert1(!hasVariable(variable))
    domains.setDomain(variable, variable.initialDomain)
    if (variable.ref.nonEmpty)
      varStore.setVariableForRef(variable.ref.get, variable)
    variableAdded(variable)
    consistent
  }

  /** Records an event notifying of the variable addition + some sanity checks */
  def variableAdded(variable: IVar) {
    variable match {
      case v: IntVariable => assert2(domains.recorded(v), "Variable has no domain")
      case _              =>
    }
    addEvent(NewVariableEvent(variable))
  }

  def addEvent(event: Event): Update = {
    logger.debug(s"  new-event: $event")
    events += event
    consistent
  }

  def hasVariable(variable: IntVariable): Boolean = domains.recorded(variable)

  def nextVarId() = VariableStore.nextVariableId()

  /** Indicates which of the two CSP (this and that) should be handled first. */
  override def compare(that: CSP): Int =
    math.signum(that.heuristic.priority - this.heuristic.priority).toInt

  def report: String = {
    val str = new StringBuilder
    val vars =
      constraints.all.flatMap(c => c.variables(csp)).collect { case v: VarWithDomain => v }
    for (v <- vars) v match {
      case v: TypedVariable[_] => str.append(s"$v = ${v.dom}\n")
      case _                   => str.append(s"$v = ${v.domain}\n")
    }
    str.append("%% ACTIVE CONSTRAINTS\n")
    for (c <- constraints.active.toSeq.sortBy(_.toString))
      str.append(s"$c  ${c.satisfaction}\n")

    str.append("%% SATISFIED CONSTRAINTS\n")
    for (c <- constraints.satisfied.toSeq.sortBy(_.toString))
      str.append(s"$c  ${c.satisfaction}\n")
    str.toString
  }

  def makespan: Int = dom(temporalHorizon).lb

  def isSolution: Boolean = {
    assert(events.isEmpty, "There are pending events in this CSP, can't check if it is a solution")
    constraints.active.isEmpty
  }
}
