package simple_prelude

import scala.language.higherKinds

class SeqMonad extends Monad[Seq] {
  override def fmap[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  override def pure[A](a: A): Seq[A] = Seq(a)
  // Cartesian join is the one compatible with Monad instance.
  // Zip join is appropriate for streams.
  override def joinWith[A, B, Z](fa: Seq[A], fb: Seq[B])(f: (A, B) => Z): Seq[Z] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
  override def sequence[A, Z](fas: Seq[Seq[A]])(f: Seq[A] => Z): Seq[Z] =
    throw new RuntimeException("TODO")
  override def bind[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] =
    for {
      a <- fa
      b <- f(a)
    } yield b
}
