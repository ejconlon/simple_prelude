package simple_prelude

import scala.language.higherKinds

class FunctorOps[F[_], A](fa: F[A], functor: Functor[F]) {
  def fmap[B](f: A => B): F[B] = functor.fmap(fa)(f)
}

class ApplicativeOps[F[_], A](fa: F[A], applicative: Applicative[F]) extends FunctorOps[F, A](fa, applicative) {
  def joinWith[B, Z](fb: F[B])(f: (A, B) => Z): F[Z] = applicative.joinWith(fa, fb)(f)
}

class MonadOps[F[_], A](fa: F[A], monad: Monad[F]) extends ApplicativeOps[F, A](fa, monad) {
  def bind[B](f: A => F[B]): F[B] = monad.bind(fa)(f)
}

class ContravariantOps[F[_], A](fa: F[A], contravariant: Contravariant[F]) {
  def contramap[Z](f: Z => A): F[Z] = contravariant.contramap(fa)(f)
}

class CategoryOps[K[_, _], A, B](x: K[A, B], category: Category[K]) {
  def andThen[C](y: K[B, C]): K[A, C] = category.andThen(x, y)
  def butFirst[Z](w: K[Z, A]): K[Z, B] = category.andThen(w, x)
}

class ComonadOps[W[_], A](wa: W[A], comonad: Comonad[W]) extends FunctorOps[W, A](wa, comonad) {
  def extract: A = comonad.extract(wa)
  def extend[B](f: W[A] => B): W[B] = comonad.extend(wa)(f)
}
