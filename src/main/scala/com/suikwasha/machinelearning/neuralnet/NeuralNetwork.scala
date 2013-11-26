package com.suikwasha.machinelearning.neuralnet

import com.suikwasha.machinelearning.{Vector, Machine}
import com.suikwasha.machinelearning.parceptron.Data

object XOR {

  val trainData = Array[Data](
    Data(Vector(0.0, 0.0, 1.0), -1.0),
    Data(Vector(0.0, 1.0, 1.0),  1.0),
    Data(Vector(1.0, 0.0, 1.0),  1.0),
    Data(Vector(1.0, 1.0, 1.0), -1.0)
  )

  def main(args: Array[String]) = {
    val stream = Stream.continually(trainData.toStream).flatten

    val w1 = Vector(0.5, 0.5, .5)
    val w2 = Vector(0.7, 0.7, .5)
    val hVec = Vector(0.2, 0.2, .5)

    val machine = stream.take(2000).foldLeft[Machine](new NeuralNetwork(w1, w2, hVec)){ (machine, data) =>
      machine.train(data.input, data.label)
    }

    trainData.foreach{ data =>
      println(machine.predict(data.input) * data.label >= 0)
    }
    machine.show
  }

}

class NeuralNetwork (val w1: Vector, val w2: Vector, val hVec: Vector) extends Machine {

  def predict(vec: Vector): Double = execute(vec)._1

  private def execute(vec: Vector): (Double, Vector) = {
    val gVec = Vector(sigmoid(w1 dot vec), sigmoid(w2 dot vec), .5)
    val u = sigmoid(gVec dot hVec)
    (u, gVec)
  }

  def train(vec: Vector, label: Double): Machine = {
    val (u, gVec) = execute(vec)
    if (u * label < 0) {
      val eVec = gVec.map(_ * (label - u) * u * (1 - u))
      val hVec_ = hVec + (eVec * K)
      val ekSeq1 = eVec.elems.zip(hVec.elems).map(t => t._1 * t._2)
      val ekSeq2 = ekSeq1.zip(gVec.elems).map(t => t._1 * t._2)
      val ekSeq = ekSeq2.zip(gVec.elems).map{
        t => t._1 * (1.0 - t._2)
      }
      val w1_ = w1 + Vector(vec.elems.zip(ekSeq).map(t => t._1 * t._2 * K): _*)
      val w2_ = w2 + Vector(vec.elems.zip(ekSeq).map(t => t._1 * t._2 * K): _*)
      new NeuralNetwork(w1_, w2_, hVec_)
    } else {
      this
    }
  }

  val K = 0.2

  def show(): Unit = {
    println(s"w1=$w1")
    println(s"w2=$w2")
    println(s"hidden=$hVec")
  }

  private def sigmoid(x: Double): Double = {
    1 / (1 + Math.exp(-1 * x))
  }
}
