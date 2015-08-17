package sadwally.scalab

object GuessGame extends App {

  def askMaxNumber(): Int = io.StdIn.readLine("What is the max number in the game? ").toInt

  def askGuess(): Int = io.StdIn.readLine("What is your guess? ").toInt

  def askAgain(): Boolean = io.StdIn.readLine("Do you want to play again? ").toBoolean

  val max = askMaxNumber()

  do {
    val x = util.Random.nextInt(max) + 1
    var guessed = false
    do {
      askGuess() match {
        case guess: Int if guess == x => { println("Right!"); guessed = true }
        case guess: Int if guess < x => println("Too small")
        case guess: Int if guess > x => println("Too big")
      }
    } while (!guessed)
  } while (askAgain())

}
