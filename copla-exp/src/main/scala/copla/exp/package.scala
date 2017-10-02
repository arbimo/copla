package copla

import java.io.File


package object exp {

  val defaultExpDir = new File("/home/abitmonn/work/copla/domains/copla")

  def problems(dir: File = defaultExpDir): Seq[File] = {
    if(!dir.exists())
      sys.error(s"$dir does not exist.")
    if(!dir.isDirectory)
      sys.error(s"$dir is not a directory.")

    dir.listFiles(f => f.isFile && f.getName.endsWith(".pb.anml")).sortBy(_.getName)
  }


}
