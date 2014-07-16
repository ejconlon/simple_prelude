package simple_prelude

import scala.language.higherKinds

object IdentityMonad extends Monad[Identity] {
  override def fmap[A, B](fa: Identity[A])(f: A => B): Identity[B] = f(fa)
  override def pure[A](a: A): Identity[A] = a
  override def joinWith[A, B, Z](fa: Identity[A], fb: Identity[B])(f: (A, B) => Z): Identity[Z] = f(fa, fb)
  override def sequence[A, Z](fas: Seq[Identity[A]])(f: Seq[A] => Z): Identity[Z] = f(fas)
  override def bind[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
}

class IdentityTMonad[M[_]](monad: Monad[M]) extends Monad[({type L[A] = IdentityT[M, A]})#L] {
  override def fmap[A, B](fa: IdentityT[M, A])(f: A => B): IdentityT[M, B] = monad.fmap(fa)(f)
  override def pure[A](a: A): IdentityT[M, A] = monad.pure(a)
  override def joinWith[A, B, Z](fa: IdentityT[M, A], fb: IdentityT[M, B])(f: (A, B) => Z): IdentityT[M, Z] = monad.joinWith(fa, fb)(f)
  override def sequence[A, Z](fas: Seq[IdentityT[M, A]])(f: Seq[A] => Z): IdentityT[M, Z] = monad.sequence(fas)(f)
  override def bind[A, B](fa: IdentityT[M, A])(f: A => IdentityT[M, B]): IdentityT[M, B] = monad.bind(fa)(f)
}

object IdentityTMonadTrans extends MonadTrans[({type L[M[_], A] = IdentityT[M, A]})#L] {
  override def lift[M[_], A](monad: Monad[M], m: M[A]): IdentityT[M, A] = m
  override def liftClass[M[_]](monad: Monad[M]): IdentityTMonad[M] = new IdentityTMonad[M](monad)
}
