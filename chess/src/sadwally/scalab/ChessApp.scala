package sadwally.scalab

import collection.mutable

object ChessApp extends App {

}

class ChessGame {

}

case class Position(x: Int, y: Int) {
  def >=(p: Position): Boolean = x >= p.x && y >= p.y

  def <=(p: Position): Boolean = x <= p.x && y <= p.y

  def >(p: Position): Boolean = x > p.x && y > p.y

  def <(p: Position): Boolean = x < p.x && y < p.y

  def -(p: Position): Position = Position(x - p.x, y - p.y)
}

class Board {
  private val pieces: mutable.Map[Position, Piece] = new mutable.HashMap()

  val size = Position(8, 8)

  def put(position: Position, piece: Piece): Option[Piece] = if (contains(position)) pieces.put(position, piece) else None

  def contains(position: Position): Boolean = position >= Position(0, 0) && position < size

  def get(position: Position): Option[Piece] = pieces.get(position)

  def isWayClear(positions: Traversable[Position]) = !positions.exists(get(_).isDefined)
}

abstract class Piece(val owner: Player) {
  def canMove(board: Board, from: Position, to: Position): Boolean = {
    board get from match {
      case Some(piece) if (piece eq this) && (board contains to) => ()
      case None => return false
    }

    val intermediatePositions: Option[Traversable[Position]] = board get to match {
      case Some(piece) if piece.owner == owner => None
      case Some(piece) if piece.isImmortal => None
      case Some(piece) => validateAttack(from, to)
      case None => validateMovement(from, to)
    }

    intermediatePositions.exists(board isWayClear)
  }

  def validateAttack(from: Position, to: Position): Option[Traversable[Position]] = validateMovement(from, to)

  def validateMovement(from: Position, to: Position): Option[Traversable[Position]]

  def isImmortal: Boolean = false
}

class Pawn(owner: Player) extends Piece(owner) {
  override def validateMovement(from: Position, to: Position): Option[Traversable[Position]] =
    if (to - from == Position(0, owner.attackDirection)) Some(None) else None

  override def validateAttack(from: Position, to: Position): Option[Traversable[Position]] =
    to - from match {
      case Position(dx, owner.attackDirection) if Math.abs(dx) == 1 => Some(None)
      case _ => None
    }
}

class King(owner: Player) extends Piece(owner) {
  override def validateMovement(from: Position, to: Position): Option[Traversable[Position]] =
    to - from match {
      case Position(dx, dy) if Math.abs(dx) <= 1 && Math.abs(dy) <= 1 => Some(None)
      case _ => None
    }
}

class Knight(owner: Player) extends Piece(owner) {
  override def validateMovement(from: Position, to: Position): Option[Traversable[Position]] =
    to - from match {
      case Position(dx, dy) if Math.abs(dx) * Math.abs(dy) == 2 => Some(None)
      case _ => None
    }
}

sealed trait Player {
  def attackDirection: Int

  def arrangeFigures(board: Board): Unit
}

object White extends Player {
  override def attackDirection: Int = 1

  override def arrangeFigures(board: Board): Unit = ???
}

object Black extends Player {
  override def attackDirection: Int = -1

  override def arrangeFigures(board: Board): Unit = ???
}