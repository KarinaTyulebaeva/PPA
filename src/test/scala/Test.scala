object Test extends App {
  val testProgram1 =
    "1 a := 5;\n2 b := sum(1, 4);\n3 c := random.choice(1, 5);" // ok
  val testProgram2 =
    "1 a := 5;\n1 b := sum(1, 4);\n3 c := random.choice(1, 5);" // repeating line=1
  val testProgram3 =
    "1 a := 5;\n2 b := b;\n3 c := random.choice(1, 5);" // b is not initialized
  val testProgram4 =
    "1 a := 5\n2 b := sum(1, 4);\n3 c := random.choice(1, 5);" // missing ;
  println("--------------------")
  ProgramApp.start(testProgram1)
  println("--------------------")
  ProgramApp.start(testProgram2)
  println("--------------------")
  ProgramApp.start(testProgram3)
  println("--------------------")
  ProgramApp.start(testProgram4)
}
