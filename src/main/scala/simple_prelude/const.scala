package simple_prelude

import scala.language.higherKinds

class ConstFunctor[X] extends Functor[({type L[A] = Const[X, A]})#L] {
  override def fmap[A, B](fa: Const[X, A])(f: A => B): Const[X, B] = fa
}

class ConstApplicative[X](monoid: Monoid[X]) extends ConstFunctor[X] with Applicative[({type L[A] = Const[X, A]})#L] {
  override def pure[A](a: A): Const[X, A] = monoid.zero
  override def joinWith[A, B, Z](fa: Const[X, A], fb: Const[X, B])(f: (A, B) => Z): Const[X, Z] =
    monoid.plus(fa, fb)
  override def sequence[A, Z](fas: Seq[Const[X, A]])(f: Seq[A] => Z): Const[X, Z] =
    monoid.sum(fas)
}
