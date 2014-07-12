package simple_prelude

import scala.language.higherKinds

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def joinWith[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
  def sequence[A, Z](fas: Seq[F[A]])(f: Seq[A] => Z): F[Z]
}

trait Monad[F[_]] extends Applicative[F] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait NatTrans[F[_], G[_]] {
  def trans[A](fa: F[A]): G[A]
}

trait Monoid[M] {
  def zero: M
  def plus(a: M, b: M): M
  def sum(as: TraversableOnce[M]): M = as.foldLeft(zero) { case (a, b) => plus(a, b) }
}

trait Contravariant[F[_]] {
  def contramap[A, Z](fa: F[A])(f: Z => A): F[Z]
}

trait Category[K[_, _]] {
  def id[A]: K[A, A]
  def andThen[A, B, C](x: K[A, B], y: K[B, C]): K[A, C]
}

trait Comonad[W[_]] extends Functor[W] {
  def extract[A](wa: W[A]): A
  def extend[A, B](wa: W[A])(f: W[A] => B): W[B]
}

