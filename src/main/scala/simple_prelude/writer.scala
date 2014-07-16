package simple_prelude

import scala.language.higherKinds

class WriterFunctor[W] extends Functor[({type L[A] = Writer[W, A]})#L] {
  override def fmap[A, B](fa: Writer[W, A])(f: A => B): Writer[W, B] = (f(fa._1), fa._2)
}

class WriterMonad[W](monoid: Monoid[W]) extends WriterFunctor[W] with Monad[({type L[A] = Writer[W, A]})#L] {
  override def pure[A](a: A): Writer[W, A] = (a, monoid.zero)
  override def joinWith[A, B, Z](fa: Writer[W, A], fb: Writer[W, B])(f: (A, B) => Z): Writer[W, Z] =
    (f(fa._1, fb._1), monoid.plus(fa._2, fb._2))
  override def sequence[A, Z](fas: Seq[Writer[W, A]])(f: Seq[A] => Z): Writer[W, Z] = {
    val as = fas map { _._1 }
    val ws = fas map { _._2 }
    val z = f(as)
    val w = monoid.sum(ws)
    (z, w)
  }
  override def bind[A, B](fa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
    val (b, v) = f(fa._1)
    (b, monoid.plus(fa._2, v))
  }
}

abstract class WriterTMonad[W, M[_]](
  monoid: Monoid[W],
  monad: Monad[M]
) extends ComposeApplicative[({type L[A] = Writer[W, A]})#L, M](new WriterMonad[W](monoid), monad)
  with Monad[({type L[A] = WriterT[W, M, A]})#L] {
  override def bind[A, B](fa: WriterT[W, M, A])(f: A => WriterT[W, M, B]): WriterT[W, M, B]
}
