package com.suikwasha.machinelearning.parceptron

import com.suikwasha.machinelearning.Machine
import com.suikwasha.machinelearning.Vector

object OR {

  val trainData = Array[Data](
    Data(Vector(0.0, 0.0, 1.0), -1.0),
    Data(Vector(0.0, 1.0, 1.0),  1.0),
    Data(Vector(1.0, 0.0, 1.0),  1.0),
    Data(Vector(1.0, 1.0, 1.0),  1.0)
  )

  def main(args: Array[String]) = {
    val stream = Stream.continually(trainData.toStream).flatten

    val machine = stream.take(1000).foldLeft[Machine](new Perceptron(Vector(0.5, 0.5, 1.0))){ (machine, data) =>
      machine.train(data.input, data.label)
    }

    trainData.foreach{ data =>
      println(machine.predict(data.input) * data.label >= 0)
    }
    machine.show
  }
}

object AND {

  val trainData = Array[Data](
    Data(Vector(0.0, 0.0, 1.0), -1.0),
    Data(Vector(0.0, 1.0, 1.0), -1.0),
    Data(Vector(1.0, 0.0, 1.0), -1.0),
    Data(Vector(1.0, 1.0, 1.0),  1.0)
  )

  def main(args: Array[String]) = {
    val stream = Stream.continually(trainData.toStream).flatten

    val machine = stream.take(2000).foldLeft[Machine](new Perceptron(Vector(0.5, 0.5, 0.5))){ (machine, data) =>
      machine.train(data.input, data.label)
    }

    trainData.foreach{ data =>
      println(machine.predict(data.input) * data.label >= 0)
    }
    machine.show
  }
}

object XOR {

  val trainData = Array[Data](
    Data(Vector(0.0, 0.0, 1.0), -1.0),
    Data(Vector(0.0, 1.0, 1.0),  1.0),
    Data(Vector(1.0, 0.0, 1.0),  1.0),
    Data(Vector(1.0, 1.0, 1.0), -1.0)
  )

  def main(args: Array[String]) = {
    val stream = Stream.continually(trainData.toStream).flatten

    val machine = stream.take(2000).foldLeft[Machine](new Perceptron(Vector(0.5, 0.5, 0.5))){ (machine, data) =>
      machine.train(data.input, data.label)
    }

    trainData.foreach{ data =>
      println(machine.predict(data.input) * data.label >= 0)
    }
    machine.show
  }
}

case class Data(input: Vector, label: Double)

class Perceptron (val wVec: Vector) extends Machine {

  def predict(vec: Vector): Double = wVec dot vec

  val C = 0.25

  def train(vec: Vector, label: Double): Machine =
    if (predict(vec) * label < 0) {
      new Perceptron(wVec + (vec * (C * label)))
    } else {
      new Perceptron(wVec)
    }

  def show(): Unit = println(wVec)
}

