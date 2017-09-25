package copla.constraints.meta

import copla.constraints.meta.constraints.DisjunctiveConstraint
import copla.constraints.meta.search.{BinarySearch, Solution}
import copla.constraints.meta.stn.constraint.MinDelay
import copla.constraints.meta.stn.variables.TemporalInterval
import copla.constraints.meta.types.statics.{BaseType, TypedVariable}
import copla.constraints.meta.variables.IntVariable
import org.scalatest.FunSuite

class JobShopWithTypesTest extends FunSuite {

  val instance = new JobShopInstance(4, List(List(2, 4, 2, 1), List(5, 3, 2), List(3, 5, 7)), Some(14))
//  val instance = new JobShopInstance(2, List(List(2, 4), List(4, 3, 3)), None) // very simple instance to avoid taking time in unit tests

  test("job shop search with types") {
    val (model, jobs) = jobShopModel(instance)
    BinarySearch.count = 0

    implicit val csp = BinarySearch.search(model, optimizeMakespan = true) match {
      case Solution(sol) => sol
      case _ => throw new AssertionError("no solution returned")
    }
    assert(csp.isSolution)
    assert(instance.optimalMakespan.isEmpty || csp.makespan == instance.optimalMakespan.get)

    // println(csp.report)
    for((m, js) <- jobs.groupBy(_.machine.dom.head).toList.sortBy(_._1)) {
      print(s"$m: ")
      val localJobs = js.sortBy(_.interval.start.domain.lb)
      println(localJobs.map(j => s"${j.interval.start.domain.lb}[${j.duration}]:(${j.jobNumber}, ${j.numInJob})").mkString("  --  "))
    }
    println("Makespan: "+csp.temporalHorizon.domain.lb)
    println("Num nodes: " + BinarySearch.count)
    println("Num constraints: "+csp.constraints.satisfied.size)
  }

  def jobShopModel(instance: JobShopInstance) : (CSP, Seq[JobWithType]) = {
    val t = BaseType("Machine", (1 to instance.numMachines).map("machine "+_))
    implicit val csp = new CSP
    val jobs =
      for(i <- instance.jobs.indices ; j <- instance.jobs(i).indices) yield {
        val int = new TemporalInterval(csp.varStore.getTimepoint(), csp.varStore.getTimepoint())
        val machine =  new TypedVariable(s"machine($i,$j)", t)

        new JobWithType(i, j, instance.jobs(i)(j), int, machine)
      }

    // set temporal constraints
    for(i <- jobs.indices) {
      val job = jobs(i)
      csp.post(job.interval.duration === job.duration -1)
      if(job.numInJob >= 1)
        csp.post(jobs(i-1).interval < jobs(i).interval)
    }

    for(j1 <- jobs ; j2 <- jobs ; if j1 != j2) {
      csp.post(new Threat(j1, j2))
    }
    (csp, jobs)
  }

  class Threat(j1: JobWithType, j2: JobWithType) extends
    DisjunctiveConstraint(List(j1.machine =!= j2.machine, j1.interval < j2.interval, j1.interval > j2.interval))
  {
    require(j1 != j2)
    override def toString = s"threat($j1, $j2)"
  }
}


case class JobWithType(jobNumber: Int, numInJob: Int, duration: Int, interval: TemporalInterval, machine: TypedVariable[String]) {
  override def toString = s"j($jobNumber, $numInJob)"
}


