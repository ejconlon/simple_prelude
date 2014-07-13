package simple_prelude

import scala.language.higherKinds

object OptionMonad extends Monad[Option] {
  override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  override def pure[A](a: A): Option[A] = Option(a)
  override def joinWith[A, B, Z](fa: Option[A], fb: Option[B])(f: (A, B) => Z): Option[Z] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
  override def sequence[A, Z](fas: Seq[Option[A]])(f: Seq[A] => Z): Option[Z] = {
    val as = fas.flatten
    if (as.isEmpty) None
    else Some(f(as))
  }
  override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
    for {
      a <- fa
      b <- f(a)
    } yield b
}

class OptionTMonad[M[_]](monad: Monad[M]) extends Monad[({type L[A] = OptionT[M, A]})#L] {
  override def fmap[A, B](fa: OptionT[M, A])(f: A => B): OptionT[M, B] =
    monad.fmap(fa) { oa => OptionMonad.fmap(oa)(f) }
  def pure[A](a: A): OptionT[M, A] = monad.pure(OptionMonad.pure(a))
  def joinWith[A, B, Z](fa: OptionT[M, A], fb: OptionT[M, B])(f: (A, B) => Z): OptionT[M, Z] =
    monad.joinWith(fa, fb) { case (oa, ob) => OptionMonad.joinWith(oa, ob)(f) }
  def sequence[A, Z](fas: Seq[OptionT[M, A]])(f: Seq[A] => Z): OptionT[M, Z] =
    monad.sequence(fas) { oas => OptionMonad.sequence(oas)(f) }
  def bind[A, B](fa: OptionT[M, A])(f: A => OptionT[M, B]): OptionT[M, B] =
    monad.bind(fa) { oa =>
      oa match {
        case None => monad.pure(None)
        case Some(a) => f(a)
      }
    }
}
