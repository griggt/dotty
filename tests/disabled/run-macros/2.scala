object Test {
  def main(args: Array[String]): Unit =
    assert(Macro.foo[Box] == 1)
}
