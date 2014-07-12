package simple_prelude

import scala.language.higherKinds

class OptionMonad extends Monad[Option] {
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
