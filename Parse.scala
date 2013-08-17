case class Tag(lemma: String, pos: String)

case class TaggedToken(spelling: String, tag: Tag) {
  def tabString = spelling + "\t" + tag.pos + "\t" + tag.lemma
}

object Parse extends App {
  val sourceDir = "../tmp_tagged/"
  val targDir = "../tagged/"
  val files = new java.io.File(sourceDir).listFiles map (_.getName)

  files.par foreach { e => save(targDir + e, parse(sourceDir + e)) }

  def parse(fname: String) = {
    println("Parsing " + fname)

    val orthre = """<orth>(.*)</orth>.*""".r
    val disambre = """.*disamb.*<base>(.*)</base><ctag>(.*)</ctag>.*""".r
    val data = scala.io.Source.fromFile(fname).mkString
    val orth =  orthre.findAllIn(data).matchData map { e => e.group(1) } 
    val disamb = disambre.findAllIn(data).matchData map { e => Tag(e.group(1), e.group(2)) }
    orth zip disamb map { e => e match {
      case (a, b) => TaggedToken(a, b)
    } }
  }

  def save(fname: String, tagged: Iterator[TaggedToken]) = {
     val p = new java.io.PrintWriter(fname)
     tagged foreach { e => p.println(e.tabString) }
     p.close()
  }
}
