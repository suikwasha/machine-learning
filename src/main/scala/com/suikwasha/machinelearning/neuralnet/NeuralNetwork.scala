package com.suikwasha.machinelearning.neuralnet

import com.suikwasha.machinelearning.Machine

/**
 * Created with IntelliJ IDEA.
 * User: suikwasha
 * Date: 2013/11/19
 * Time: 23:02
 * To change this template use File | Settings | File Templates.
 */



object NeuralNetwork extends Machine {



  def predict(wVec: Seq[Double], xVec: Seq[Double]): Double = {

  }

  def train(wVec: Seq[Double], xVec: Seq[Double], label: Double): Seq[Double] = {
    0
  }

  private def sigmoid(x: Double): Double =  1 / (1 + Math.exp(-1.0 * x))
}
