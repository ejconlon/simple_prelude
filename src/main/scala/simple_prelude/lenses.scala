package simple_prelude

import scala.language.higherKinds

trait Traversal[S, T, A, B] {
  def traversal[F[_]](f: A => F[B], applicative: Applicative[F]): (S => F[T])

  final def traversalLike[F[_]](applicative: Applicative[F]): LensLike[F, S, T, A, B] = { f =>
    traversal(f, applicative)
  }
}

trait Lens[S, T, A, B] extends Traversal[S, T, A, B] {
  def lens[F[_]](f: A => F[B], functor: Functor[F]): (S => F[T])

  final override def traversal[F[_]](f: A => F[B], applicative: Applicative[F]): (S => F[T]) =
    lens(f, applicative)

  final def lensLike[F[_]](functor: Functor[F]): LensLike[F, S, T, A, B] = { f =>
    lens(f, functor)
  }
}

trait IdLens[A, B] extends Lens[A, B, A, B] {
  override def lens[F[_]](f: A => F[B], functor: Functor[F]): A => F[B] = { a => f(a) }
}

trait FirstLens[A, B, C] extends Lens[(A, B), (C, B), A, C] {
  override def lens[F[_]](f: A => F[C], functor: Functor[F]): ((A, B)) => F[(C, B)] = { case (a, b) =>
    functor.fmap(f(a)) { c => (c, b) }
  }
}

trait SecondLens[A, B, C] extends Lens[(A, B), (A, C), B, C] {
  override def lens[F[_]](f: B => F[C], functor: Functor[F]): ((A, B)) => F[(A, C)] = { case (a, b) =>
    functor.fmap(f(b)) { c => (a, c) }
  }
}

object Lenses {
  type Getting[R, A, B, C, D] = LensLike[({type L[X] = Const[R, X]})#L, A, B, C, D]
  type Setter[A, B, C, D] = LensLike[Identity, A, B, C, D]

  def view[A, B, C, D](getting: Getting[C, A, B, C, D]): A => C = {
    val g: (A => Const[C, B]) = getting(id[C])
    val h: (A => C) = { (a: A) => g(a) }
    h
  }

  def set[A, B, C, D](setter: Setter[A, B, C, D]): D => A => B = { (d: D) =>
    val g: (A => Identity[B]) = setter(const[C, D](d))
    g
  }
}
