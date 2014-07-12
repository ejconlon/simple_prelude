package simple_prelude

import scala.language.higherKinds

sealed trait FunctorOp[F[_], Z] extends ApplicativeOp[F, Z] {
  def interpret(functor: Functor[F]): F[Z] =
    this match {
      case FMap(fa, f) =>
        functor.fmap(fa)(f)
    }

  override def transform[G[_]](natTrans: NatTrans[F, G]): FunctorOp[G, Z] =
    this match {
      case FMap(fa, f) =>
        FMap(natTrans.trans(fa), f)
    }
}

sealed trait ApplicativeOp[F[_], Z] extends MonadOp[F, Z] {
  def interpret(applicative: Applicative[F]): F[Z] =
    this match {
      case FMap(fa, f) =>
        applicative.fmap(fa)(f)
      case Pure(fz) =>
        fz
      case JoinWith(fa, fb, f) =>
        applicative.joinWith(fa, fb)(f)
      case Sequence(fas, f) =>
        applicative.sequence(fas)(f)
    }

  override def transform[G[_]](natTrans: NatTrans[F, G]): ApplicativeOp[G, Z] =
    this match {
      case FMap(fa, f) =>
        FMap(natTrans.trans(fa), f)
      case Pure(fz) =>
        Pure(natTrans.trans(fz))
      case JoinWith(fa, fb, f) =>
        JoinWith(natTrans.trans(fa), natTrans.trans(fb), f)
      case Sequence(fas, f) =>
        Sequence(fas map { natTrans.trans(_) }, f)
    }
}

sealed trait MonadOp[F[_], Z] {
  def interpret(monad: Monad[F]): F[Z] =
    this match {
      case FMap(fa, f) =>
        monad.fmap(fa)(f)
      case Pure(fz) =>
        fz
      case JoinWith(fa, fb, f) =>
        monad.joinWith(fa, fb)(f)
      case Sequence(fas, f) =>
        monad.sequence(fas)(f)
      case Bind(fi, f) =>
        monad.bind(fi)(f)
    }

  def transform[G[_]](natTrans: NatTrans[F, G]): MonadOp[G, Z] =
    this match {
      case FMap(fa, f) =>
        FMap(natTrans.trans(fa), f)
      case Pure(fz) =>
        Pure(natTrans.trans(fz))
      case JoinWith(fa, fb, f) =>
        JoinWith(natTrans.trans(fa), natTrans.trans(fb), f)
      case Sequence(fas, f) =>
        Sequence(fas map { natTrans.trans(_) }, f)
      case Bind(fi, f) =>
        Bind(natTrans.trans(fi), { (i: Any) =>  // erasure...
          val fz = f(i)
          natTrans.trans(fz)
        })
    }
}

case class FMap[F[_], A, Z](fa: F[A], f: A => Z) extends FunctorOp[F, Z]
case class Pure[F[_], Z](fz: F[Z]) extends ApplicativeOp[F, Z]
case class JoinWith[F[_], A, B, Z](fa: F[A], fb: F[B], f: (A, B) => Z) extends ApplicativeOp[F, Z]
case class Sequence[F[_], A, B, Z](fas: Seq[F[A]], f: Seq[A] => Z) extends ApplicativeOp[F, Z]
case class Bind[F[_], I, Z](fi: F[I], f: I => F[Z]) extends MonadOp[F, Z]
