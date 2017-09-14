package copla.planning.model

import copla.lang.model.core._

class Problem(val anml: Seq[InModuleBlock]) {

  // FIXME: ID is ignore when looking up the start timepoint
  val start: TPRef = anml
    .collectFirst { case TimepointDeclaration(tp) if tp.id.name == "start" => tp }
    .getOrElse(sys.error("No start timepoint in ANML module"))
  val end: TPRef = anml
    .collectFirst { case TimepointDeclaration(tp) if tp.id.name == "end" => tp }
    .getOrElse(sys.error("No end timepoint in ANML module"))

}
