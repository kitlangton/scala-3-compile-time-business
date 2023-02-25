package lab.pages
import scala.compiletime.*
import lab.macros.debug
import lab.macros.debugInline

// ================================================================================
// Inline If
// ================================================================================

object Config:
  inline val logging = false

object Logger:

  inline def log[T](inline msg: String)(inline op: T): T =
    inline if Config.logging then actuallyLog(msg)(op)
    else op

  private def actuallyLog[T](msg: String)(op: => T): T =
    println(s"start $msg".cyan)
    val result = op
    println(s"$msg = $result".cyan)
    result

// ================================================================================
// EXAMPLE
// ================================================================================

def importantMethod(string: String, int: Int) =
  println(s"I HAVE IMPORTANTLY CALCULATED: ${string * int}".blue)
  999

@main
def inlineLoggerExample =
  debugInline {
    Logger.log("running importart code" + scala.util.Random.nextInt(10)) {
      importantMethod("hello", 3)
    }
  }
