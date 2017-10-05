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

  private val domainsById: mutable.ArrayBuffer[Domain] = base match {
    case Some(base) => base.domainsById.clone()
    case _          => mutable.ArrayBuffer()
  }

  private val emptySpots: mutable.Set[Int] = base match {
    case Some(x) => x.emptySpots.clone()
    case _       => mutable.Set()
  }

  val variableIds: mutable.Map[IntVariable, Int] = base match {
    case Some(base) => base.variableIds.clone()
    case _          => mutable.Map()
  }

  val boundVariables: mutable.Set[IntVariable] = base match {
    case Some(x) => x.boundVariables.clone()
    case _       => mutable.Set()
  }

  def domOpt(v: IntVariable): Option[Domain] =
    variableIds
      .get(v)
      .map(id => domainsById(id))

  private def dom(v: IntVariable): Domain = domainsById(variableIds(v))

  def recorded(v: IntVariable): Boolean = variableIds.contains(v)

  def setDomain(variable: IntVariable, domain: Domain): Unit = {
    require(!recorded(variable))
    setDomainImpl(variable, domain)
  }

  private def setDomainImpl(variable: IntVariable, domain: Domain): Unit = {
    val id = variableIds.get(variable) match {
      case Some(x) => x
      case None if emptySpots.nonEmpty =>
        val newId = emptySpots.head
        emptySpots -= newId
        assert3(domainsById(newId) == null)
        variableIds(variable) = newId
        newId
      case _ =>
        val newId = domainsById.size
        variableIds(variable) = newId
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
      fatal("empty domain update")
    } else if (!recorded(variable)) {
      setDomainImpl(variable, newDomain)
      consistent(Nil)
    } else if (dom(variable).size > newDomain.size) {
      setDomainImpl(variable, newDomain)
      consistent(Seq(DomainReduced(variable)))
    } else if (dom(variable).size < newDomain.size && !isBound(variable)) {
      // domain increase and there is no bindng constraint forbiding its growth
      setDomainImpl(variable, newDomain)
      consistent(Seq(DomainExtended(variable)))
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

  private def varsWithId(id: Int): Set[IntVariable] = variableIds.toSeq.filter(_._2 == id).map(_._1).toSet

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
        val changedVariables =
          (if(commonDomain.size < dom(left).size) varsWithId(lid) else Set()) ++
            (if(commonDomain.size != dom(right).size) varsWithId(rid) else Set())

        for(v <- varsWithId(rid))
          variableIds(v) = lid
        assert3(varsWithId(rid).isEmpty)
        emptySpots += rid
        domainsById(rid) = null
        domainsById(lid) = commonDomain

        foreach(changedVariables)(v => csp.addEvent(DomainReduced(v)))
      case _ =>
        consistent
    }
    case _ =>
      consistent
  }

}
