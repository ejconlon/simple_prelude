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

class OptionTMonad[M[_]](
  monad: Monad[M]
) extends ComposeApplicative[Option, M](OptionMonad, monad)
  with Monad[({type L[A] = OptionT[M, A]})#L] {
  override def bind[A, B](fa: OptionT[M, A])(f: A => OptionT[M, B]): OptionT[M, B] =
    monad.bind(fa) { oa =>
      oa match {
        case None => monad.pure(None)
        case Some(a) => f(a)
      }
    }
}

object OptionTMonadTrans extends MonadTrans[({type L[M[_], A] = OptionT[M, A]})#L] {
  override def lift[M[_], A](monad: Monad[M], m: M[A]): OptionT[M, A] = monad.fmap(m) { Some(_) }
  override def liftClass[M[_]](monad: Monad[M]): OptionTMonad[M] = new OptionTMonad[M](monad)
}
