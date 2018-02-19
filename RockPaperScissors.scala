package com.github.edgecaseberg

import scala.io.StdIn
import scala.util.{Try, Success}
import scala.util.Random

sealed trait DuelResult
case object Win extends DuelResult
case object Lost extends DuelResult
case object Draw extends DuelResult

object Command {
	implicit class CommandOps(c: Command) {
		def beats(other: Command): DuelResult = {
			(c, other) match {
				case (Scissors, Rock) => Lost
				case (Scissors, Paper) => Win
				case (Rock, Scissors) => Win
				case (Rock, Paper) => Lost
				case (Paper, Scissors) => Lost
				case (Paper, Rock) => Win
				case _ => Draw 
			}

		}
		def beatenBy = {
			all.filter(x => x.beats(c) == Win).head
		}
	}
	def all: Set[Command] = Set(Scissors, Rock, Paper)
}

sealed trait Command 

case object Scissors extends Command 
case object Rock extends Command 
case object Paper extends Command

sealed abstract class Contender {
	var history: Seq[Command] = Nil

	def decideCommand(): Command

	def clearHistory() {
		history = Nil
	}
}

class HumanContender extends Contender {
	private val validCommands = Map('s' -> Scissors,'r' -> Rock,'p' -> Paper)
		
	def decideCommand(): Command = {
		var validCommand = Option.empty[Command]
		while(validCommand.isEmpty) {
			println("Please enter command: [r]ock, [p]aper, or [s]cissors")
			validCommand = Try(StdIn.readChar()) match {
				case Success(c) if validCommands.contains(c.toLower) => {
					validCommands.get(c.toLower)
				}
				case _ =>
					println("Sorry! I didn't understand!")
					Option.empty[Command]
			}
		}
		history = validCommand.get +: history 
		history.head
	}
}

class ComputerContender(
	opponent: Contender, 
	preferredCommand: Command
) extends Contender {
	
	def decideCommand(): Command = {
		val countsPerCommand = opponent.history.groupBy(identity)
		val mostLikelyCommandToThrow = Try(countsPerCommand.maxBy {
			case (command, numTimesUsed) => numTimesUsed.size
		}).map {
			case (command, numTimesUsed) => command.beatenBy
		}.getOrElse(preferredCommand)

		val otherCommands = Command.all - mostLikelyCommandToThrow

		val otherChances = otherCommands.flatMap { command =>
			Vector.fill(25)(command)
		}
		
		val commandsToChooseFrom = Vector.fill(50)(mostLikelyCommandToThrow) ++ otherChances

		val choice = Random.nextInt(commandsToChooseFrom.size)

		commandsToChooseFrom(choice)
	}
}

class Game(numGamesToPlay: Int) {
	val h = new HumanContender
	val c = new ComputerContender(h, Command.all.toList(Random.nextInt(Command.all.size)))

	var gamesPlayed = 0

	def startMatch() = {
		println("Welcome to Rock Paper Scissors!")
		println("Size up your opponent, and let's go!")

		while(gamesPlayed < numGamesToPlay) {
			h.decideCommand().beats(c.decideCommand()) match {
				case Win => 
					println("Congrats! You won!")
					println("Get ready for the next round!")
				case Lost =>
					println("Oh, too bad!")
				case Draw =>
					println("Draw! Looks like you're evenly matched!")
			}
			gamesPlayed = gamesPlayed + 1
		}

		println("Thanks for playing!")
		gamesPlayed = 0
		
	}
}