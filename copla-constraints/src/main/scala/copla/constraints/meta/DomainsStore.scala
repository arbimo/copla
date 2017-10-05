package copla.constraints.meta

import copla.constraints.meta.domains.Domain
import copla.constraints.meta.events._
import copla.constraints.meta.updates._
import copla.constraints.meta.util.Assertion._
import copla.constraints.meta.variables.{IntVariable, VarWithDomain}

import scala.collection.mutable

class DomainsStore(base: Option[DomainsStore] = None) extends InternalCSPEventHandler with slogging.LazyLogging {

  val domainsById: mutable.ArrayBuffer[Domain] = base match {
    case Some(base) => base.domainsById.clone()
    case _ => mutable.ArrayBuffer()
  }

  val variableIds: mutable.Map[VarWithDomain, Int] = base match {
    case Some(base) => base.variableIds.clone()
    case _           => mutable.Map()
  }

  def domOpt(v: IntVariable): Option[Domain] =
    variableIds.get(v)
      .map(id => domainsById(id))

  private def dom(v: IntVariable): Domain = domainsById(variableIds(v))

  def recorded(v: IntVariable): Boolean = variableIds.contains(v)

  def setDomain(variable: IntVariable, domain: Domain): Unit = {
    require(!recorded(variable))
    setDomainImpl(variable, domain)
  }

  private def setDomainImpl(variable: IntVariable, domain: Domain): Unit = {
    val id =
      variableIds.getOrElseUpdate(variable, { domainsById.size })
    assert2(id <= domainsById.size)

    if(id == domainsById.size)
      domainsById += domain
    else
      domainsById(id) = domain
  }

  def updateDomain(variable: IntVariable, newDomain: Domain): UpdateResult[Seq[DomainChange]] = {
    logger.debug(s"  dom-update: $variable <- $newDomain")
    if (newDomain.isEmpty) {
      fatal("empty domain update")
    } else if(!recorded(variable)) {
      setDomainImpl(variable, newDomain)
      consistent(Nil)
    } else if (dom(variable).size > newDomain.size) {
      setDomainImpl(variable, newDomain)
      consistent(Seq(DomainReduced(variable)))
    } else if (dom(variable).size < newDomain.size) { // TODO merge with bindings: && !bindings.isBound(variable)) {
      // domain increase and there is no bindng constraint forbiding its growth
      setDomainImpl(variable, newDomain)
      consistent(Seq(DomainExtended(variable)))
    } else {
      assert3(dom(variable) == newDomain)
      consistent(Nil)
    }
  }


  /** Invoked when a CSP is cloned, the new CSP will append the handler resulting from this method into its own handlers */
  override def clone(newCSP: CSP): InternalCSPEventHandler =
    new DomainsStore(Some(this))

  override def handleEvent(event: Event): Update = consistent // TODO

}
