package apg

import java.io.File

import ammonite.ops.Path
import ammonite.util.Res.{Exception, Failure}

object Main extends App {

  ammonite.Main(predef = s"val args = ${args.tail.map(escape).mkString("Array(", ", ", ")")}", verboseOutput = true).runScript(Path(new File(args(0)).getAbsolutePath), Seq.empty, Seq.empty) match {
    case Failure(ex, msg) =>
      ex match {
        case Some(t) => throw t
        case None => throw new RuntimeException(msg)
      }
    case Exception(t, msg) => throw t
    case _ =>
  }

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

}
