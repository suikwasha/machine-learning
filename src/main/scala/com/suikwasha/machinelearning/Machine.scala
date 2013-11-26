package com.suikwasha.machinelearning

trait Machine {
  def predict(vec: Vector): Double
  def train(vec: Vector, label: Double): Machine
  def show(): Unit
}

case class Vector(elems: Double *) {

  lazy val length: Int = elems.length

  def dot (vec: Vector): Double = {
    require(vec.length == length)
    elems.zip(vec.elems).map(t => t._1 * t._2).reduce(_ + _)
  }

  def * (n: Double): Vector = Vector(elems.map(_ * n): _*)

  def + (vec: Vector): Vector = {
    require(vec.length == length)
    Vector(elems.zip(vec.elems).map(t => t._1 + t._2): _*)
  }

  def map(f: (Double) => Double): Vector = Vector(elems.map(f): _*)
}