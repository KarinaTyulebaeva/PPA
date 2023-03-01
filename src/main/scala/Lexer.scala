import org.parboiled2._

object storage {
  var variableMap: scala.collection.mutable.Map[String, Int] =
    scala.collection.mutable.Map.empty

  var program: scala.collection.mutable.ListBuffer[(Int, (String, Int))] =
    scala.collection.mutable.ListBuffer.empty
}

class Program(val input: ParserInput) extends Parser {

  var randomVariableStack: scala.collection.mutable.Set[Int] =
    scala.collection.mutable.Set.empty

  def Program = rule(Line ~ oneOrMore(";\n" ~ Line) ~ ";" | Line ~ ";")

  def Line = rule(Integer ~ " " ~ Command ~> ((line, command) => {
    storage.program.addOne((line, command))
  }))

  def Command =
    rule(
      Var ~ " := " ~ Integer ~> ((a, b) => {
        storage.variableMap.update(a, b)
        (a, b)
      })
        | Var ~ " := " ~ Sum ~> ((a, b) => {
          storage.variableMap.addOne((a, b))
          (a, b)
        })
        | Var ~ " := " ~ Random ~> ((a, b) => {
          storage.variableMap.addOne((a, b))
          (a, b)
        })
        | Var ~ " := " ~ Var ~> ((a, b) => {
          storage.variableMap.get(b) match {
            case Some(value) => {
              storage.variableMap.addOne(a, value)
              (a, value)
            }
            case None => {
              storage.variableMap.addOne((s"invalid $a", -1))
              (s"invalid $a", -1)
            }
          }
        })
    )

  def Expression = rule { Var | Integer | Sum | Random }

  def Var = rule(capture(Alpha) ~> (_.toString))

  def Alpha = rule(oneOrMore(CharPredicate.Alpha))

  def Sum = rule {
    "sum(" ~ Integer ~ zeroOrMore(
      ", " ~ Integer ~> ((_: Int) + _)
    ) ~ ")" |
      "sum(" ~ Integer ~ zeroOrMore(
        ", " ~ Random ~> ((_: Int) + _)
      ) ~ ")"
  }

  def Random = rule {
    "random.choice(" ~ Integer ~ zeroOrMore(
      ", " ~ Integer ~> ((a: Int, b) => {
        randomVariableStack.add(a)
        randomVariableStack.add(b)
        a + b
      })
    ) ~ ")" ~> (_ => {
      var h = randomVariableStack.head
      randomVariableStack.empty
      h
    })
  }

  def Integer = rule { capture(Digits) ~> (_.toInt) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

object ProgramApp {

  def logRepeatedLines(
      program: scala.collection.mutable.ListBuffer[(Int, (String, Int))]
  ) = program.groupBy(line => line._1).collect {
    case (x, ys) if ys.lengthCompare(1) > 0 => println(s"line $x is repeated")
  }

  def checkRepeatedLines(
      program: scala.collection.mutable.ListBuffer[(Int, (String, Int))]
  ) = {
    val uniqueSize = storage.program.distinctBy(line => line._1).size
    if (uniqueSize < storage.program.size) {
      logRepeatedLines(program)
      true
    } else false
  }

  def checkInvalidVariables = {
    checkRepeatedLines(storage.program)
    storage.variableMap.collect { case (variable, _) =>
      if (variable.contains("invalid"))
        println(
          s"Invalid variables assign for: ${variable.drop(8)}"
        )
    }
  }

  def start(program: String) = {
    val res =
      new Program(
        program
      ).Program
        .run()
    println(res)
    checkInvalidVariables

    storage.program.clear()
    storage.variableMap.clear()
  }

}
