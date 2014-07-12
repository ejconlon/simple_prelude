package simple_prelude

import scala.language.higherKinds

import scala.util.{Failure, Success, Try}

class TryMonad extends Monad[Try] {
  override def fmap[A, B](fa: Try[A])(f: A => B): Try[B] =
    fa match {
      case l@Failure(_) => l.asInstanceOf[Try[B]]
      case Success(a) => Success(f(a))
    }
  override def pure[A](a: A): Try[A] = Success(a)
  override def joinWith[A, B, Z](fa: Try[A], fb: Try[B])(f: (A, B) => Z): Try[Z] =
    (fa, fb) match {
      case (l@Failure(_), _) => l.asInstanceOf[Try[Z]]
      case (_, l@Failure(_)) => l.asInstanceOf[Try[Z]]
      case (Success(a), Success(b)) => Success(f(a, b))
    }
  override def sequence[A, Z](fas: Seq[Try[A]])(f: Seq[A] => Z): Try[Z] = {
    val as = Seq.newBuilder[A]
    fas.foreach { fa =>
      fa match {
        case l@Failure(_) => return l.asInstanceOf[Try[Z]]
        case Success(a) => as += a
      }
    }
    Success(f(as.result))
  }
  override def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
    fa match {
      case l@Failure(_) => l.asInstanceOf[Try[B]]
      case Success(a) => f(a)
    }
}
