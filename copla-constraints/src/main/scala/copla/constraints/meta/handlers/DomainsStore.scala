package copla.constraints.meta.handlers

import copla.constraints.meta.CSP
import copla.constraints.meta.constraints.{BindConstraint, EqualityConstraint}
import copla.constraints.meta.domains.Domain
import copla.constraints.meta.events._
import copla.constraints.meta.updates._
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.{IntVariable, VarWithDomain}

import scala.collection.mutable

class DomainsStore(csp: CSP, base: Option[DomainsStore] = None)
    extends InternalCSPEventHandler
    with slogging.LazyLogging {

  type DomainID = Int

  private val domainsById: mutable.ArrayBuffer[Domain] = base match {
    case Some(base) => base.domainsById.clone()
    case _          => mutable.ArrayBuffer()
  }

  /** Tracks which of the domains are ID are free. A free domain ID is materialized by a null value in the
    * domainsById array. This ID might be reused for a new variable. */
  private val emptySpots: mutable.Set[DomainID] = base match {
    case Some(x) => x.emptySpots.clone()
    case _       => mutable.Set()
  }

  /** Associates each variable with a given domain ID which can be used
    * (i) to retrieve its domain from domainsById.
    * (ii) to test for entailed equality (two variables with the same domain ID where enforced to be equal. */
  private val variableIds: mutable.Map[IntVariable, DomainID] = base match {
    case Some(base) => base.variableIds.clone()
    case _          => mutable.Map()
  }

  private val variablesById: mutable.Map[DomainID, mutable.Set[IntVariable]] = base match {
    case Some(x) => mutable.Map(x.variablesById.toSeq.map(p => (p._1, p._2.clone())): _*)
    case _ => mutable.Map()
  }

  private val boundVariables: mutable.Set[IntVariable] = base match {
    case Some(x) => x.boundVariables.clone()
    case _       => mutable.Set()
  }

  /** Returns true if the two variables are subject to an equality constraint. */
  def enforcedEqual(v1: IntVariable, v2: IntVariable): Boolean =
    recorded(v1) && recorded(v2) && id(v1) == id(v2)

  def domOpt(v: IntVariable): Option[Domain] =
    variableIds
      .get(v)
      .map(id => domainsById(id))

  /** Returns the domain of a variable. Throws if the variable has no recorded domain. */
  private def dom(v: IntVariable): Domain = domainsById(variableIds(v))

  /** Returns true if the variable has been previously recorded (i.e. is associated with a domain). */
  def recorded(v: IntVariable): Boolean = variableIds.contains(v)

  /** Set the domain of the given variable.
    * Require that the variable was not previously recorded and that domain is not empty.
    * When this is not provable, one should use updateDomain instead. */
  def setDomain(variable: IntVariable, domain: Domain): Unit = {
    require(!recorded(variable))
    require(domain.nonEmpty)
    setDomainImpl(variable, domain)
  }

  private def setId(variable: IntVariable, newId: DomainID): Unit = {
    if(variableIds.contains(variable))
      variablesById(id(variable)) -= variable
    variableIds(variable) = newId
    variablesById.getOrElseUpdate(newId, mutable.Set()) += variable
  }

  private def setDomainImpl(variable: IntVariable, domain: Domain): Unit = {
    val id = variableIds.get(variable) match {
      case Some(x) => x
      case None if emptySpots.nonEmpty =>
        val newId = emptySpots.head
        emptySpots -= newId
        assert3(domainsById(newId) == null)
        setId(variable, newId)
        newId
      case _ =>
        val newId = domainsById.size
        setId(variable, newId)
        newId
    }
    assert2(id <= domainsById.size)

    if (id == domainsById.size)
      domainsById += domain
    else
      domainsById(id) = domain
  }

  def updateDomain(variable: IntVariable, newDomain: Domain): UpdateResult[Seq[DomainChange]] = {
    logger.debug(s"  dom-update: $variable <- $newDomain")
    if (newDomain.isEmpty) {
      inconsistent(s"Empty domain update for variable $variable")
    } else if (!recorded(variable)) {
      setDomainImpl(variable, newDomain)
      consistent(Nil)
    } else if (dom(variable).size > newDomain.size) {
      setDomainImpl(variable, newDomain)
      consistent(varsWithId(id(variable)).map(DomainReduced(_)))
    } else if (dom(variable).size < newDomain.size && !isBound(variable)) {
      // domain increase and there is no bindng constraint forbiding its growth
      setDomainImpl(variable, newDomain)
      consistent(varsWithId(id(variable)).map(DomainExtended(_)))
    } else {
      assert3(isBound(variable) || dom(variable) == newDomain)
      consistent(Nil)
    }
  }

  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  override def clone(newCSP: CSP): InternalCSPEventHandler =
    new DomainsStore(newCSP, Some(this))

  def isBound(variable: IntVariable): Boolean = boundVariables.contains(variable)

  private def id(v: IntVariable): Int = variableIds(v)

  private def varsWithId(id: DomainID): Seq[IntVariable] = {
    assert3(variableIds.toSeq.filter(_._2 == id).map(_._1).toSet == variablesById(id))
    variablesById(id).toSeq
  }

  override def handleEvent(event: Event): Update = event match {
    case NewConstraint(c: BindConstraint) =>
      boundVariables += c.variable
      consistent
    case NewConstraint(x: EqualityConstraint) => (x.v1, x.v2) match {
      case (left: IntVariable, right: IntVariable) if left != right && recorded(left) && recorded(right) && id(left) != id(right) =>
        // merge
        val lid = id(left)
        val rid = id(right)
        val commonDomain = dom(left).intersection(dom(right))
        if(commonDomain.isEmpty) {
          inconsistent(s"Equality constraint $x resulted in ${x.v1} and ${x.v2} having an empty domain.")
        } else {
          val changedVariables =
            (if (commonDomain.size < dom(left).size) varsWithId(lid) else Set()) ++
              (if (commonDomain.size != dom(right).size) varsWithId(rid) else Set())

          for (v <- varsWithId(rid)) {
            setId(v, lid)
          }
          assert3(varsWithId(rid).isEmpty)
          emptySpots += rid
          domainsById(rid) = null
          domainsById(lid) = commonDomain

          foreach(changedVariables)(v => csp.addEvent(DomainReduced(v)))
        }
      case _ =>
        consistent
    }
    case _ =>
      consistent
  }
}
