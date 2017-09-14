package copla.lang.model.transforms

case class Config(
    /** If set to true, CoPla will try to merge equal timepoints into a single one. */
    mergeTimepoints: Boolean = true)
