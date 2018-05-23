package Quiz

import cats.Monoid
import exam.ℤ.∆

sealed trait R2[A] {
  type P = A Tuple2 A
  type I = (A) => A
  type ∆ = (P) => P

  def Ø: P
  def → : ∆
  def ↤ : ∆
  def ↓ : ∆
  def ↑ : ∆
  def ↗ : ∆
  def ↘ : ∆
  def ↖ : ∆
  def ↙ : ∆
  def ⇌ : ∆

  def δ(f: I)(g: I): ∆ = { case (x, y) => ??? }

  implicit def rm: Monoid[∆] = new Monoid[∆] {
    override def empty: ∆ = ⇌
    override def combine(f: ∆, g: ∆): ∆ = f g
  }
}

case object ℤ extends R2[Int] {
  override def Ø: P = (0, 0)
  override def → : ∆ = δ(_ + 1)(_ + 0)
  override def ↤ : ∆ = δ(_ - 1)(_ + 0)
  override def ↓ : ∆ = δ(_ + 0)(_ - 1)
  override def ↑ : ∆ = δ(_ + 0)(_ + 1)
  override def ↗ : ∆ = δ(_ + 1)(_ + 1)
  override def ↘ : ∆ = δ(_ + 1)(_ - 1)
  override def ↖ : ∆ = δ(_ - 1)(_ + 1)
  override def ↙ : ∆ = δ(_ - 1)(_ - 1)
  override def ⇌ = δ(identity)(identity)
  def move: (P) => ∆ = { ??? }
}