package simple_prelude

import scala.language.higherKinds

trait FirstMonoid[A] extends Monoid[Option[A]] {
  override def zero = None
  override def plus(a: Option[A], b: Option[A]): Option[A] =
    a match {
      case Some(x) => a
      case _ => b
    }
}

trait LastMonoid[A] extends Monoid[Option[A]] {
  override def zero = None
  override def plus(a: Option[A], b: Option[A]): Option[A] =
    b match {
      case Some(x) => b
      case _ => a
    }
}

trait SeqMonoid[A] extends Monoid[Seq[A]] {
  override def zero = Seq.empty[A]
  override def plus(a: Seq[A], b: Seq[A]): Seq[A] = a ++ b
}
