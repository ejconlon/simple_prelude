package simple_prelude

import scala.language.higherKinds

object IdentityMonad extends Monad[Identity] {
  override def fmap[A, B](fa: Identity[A])(f: A => B): Identity[B] = f(fa)
  override def pure[A](a: A): Identity[A] = a
  override def joinWith[A, B, Z](fa: Identity[A], fb: Identity[B])(f: (A, B) => Z): Identity[Z] = f(fa, fb)
  override def sequence[A, Z](fas: Seq[Identity[A]])(f: Seq[A] => Z): Identity[Z] = f(fas)
  override def bind[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
}

