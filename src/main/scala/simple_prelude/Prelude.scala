package simple_prelude

import scala.language.higherKinds
import scala.util.{Try, Success, Failure}

object Prelude {

  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def joinWith[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
    def sequence[A, Z](fas: Seq[F[A]])(f: Seq[A] => Z): F[Z]
  }

  trait Monad[F[_]] extends Applicative[F] {
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait NatTrans[F[_], G[_]] {
    def trans[A](fa: F[A]): G[A]
  }

  trait Monoid[M] {
    def zero: M
    def plus(a: M, b: M): M
    def sum(as: TraversableOnce[M]): M = as.foldLeft(zero) { case (a, b) => plus(a, b) }
  }

  class FunctorOps[F[_], A](fa: F[A], functor: Functor[F]) {
    def fmap[B](f: A => B): F[B] = functor.fmap(fa)(f)
  }

  class ApplicativeOps[F[_], A](fa: F[A], applicative: Applicative[F]) extends FunctorOps[F, A](fa, applicative) {
    def joinWith[B, Z](fb: F[B])(f: (A, B) => Z): F[Z] = applicative.joinWith(fa, fb)(f)
  }

  class MonadOps[F[_], A](fa: F[A], monad: Monad[F]) extends ApplicativeOps[F, A](fa, monad) {
    def bind[B](f: A => F[B]): F[B] = monad.bind(fa)(f)
  }

  type Identity[A] = A

  class IdentityMonad extends Monad[Identity] {
    override def fmap[A, B](fa: Identity[A])(f: A => B): Identity[B] = f(fa)
    override def pure[A](a: A): Identity[A] = a
    override def joinWith[A, B, Z](fa: Identity[A], fb: Identity[B])(f: (A, B) => Z): Identity[Z] = f(fa, fb)
    override def sequence[A, Z](fas: Seq[Identity[A]])(f: Seq[A] => Z): Identity[Z] = f(fas)
    override def bind[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
  }

  type Const[X, A] = X

  class ConstFunctor[X] extends Functor[({type L[A] = Const[X, A]})#L] {
    override def fmap[A, B](fa: Const[X, A])(f: A => B): Const[X, B] = fa
  }

  class ConstApplicative[X](monoid: Monoid[X]) extends ConstFunctor[X] with Applicative[({type L[A] = Const[X, A]})#L] {
    override def pure[A](a: A): Const[X, A] = monoid.zero
    override def joinWith[A, B, Z](fa: Const[X, A], fb: Const[X, B])(f: (A, B) => Z): Const[X, Z] =
      monoid.plus(fa, fb)
    override def sequence[A, Z](fas: Seq[Const[X, A]])(f: Seq[A] => Z): Const[X, Z] =
      monoid.sum(fas)
  }

  type Reader[R, A] = R => A

  class ReaderMonad[R] extends Monad[({type L[A] = Reader[R, A]})#L] {
    override def fmap[A, B](fa: Reader[R, A])(f: A => B): Reader[R, B] = { (r: R) => f(fa(r)) }
    override def pure[A](a: A): Reader[R, A] = { (_: R) => a }
    override def joinWith[A, B, Z](fa: Reader[R, A], fb: Reader[R, B])(f: (A, B) => Z): Reader[R, Z] = { (r: R) =>
      val a = fa(r)
      val b = fb(r)
      f(a, b)
    }
    override def sequence[A, Z](fas: Seq[Reader[R, A]])(f: Seq[A] => Z): Reader[R, Z] = { (r: R) =>
      val as = fas map { _(r) }
      f(as)
    }
    override def bind[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = { (r: R) =>
      val a = fa(r)
      val fb = f(a)
      fb(r)
    }
  }

  type Writer[W, A] = (A, W)

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

  type State[S, A] = S => (A, S)

  class StateMonad[S] extends Monad[({type L[A] = State[S, A]})#L] {
    override def fmap[A, B](fa: State[S, A])(f: A => B): State[S, B] = { (s: S) =>
      val (a, t) = fa(s)
      (f(a), t)
    }
    override def pure[A](a: A): State[S, A] = { (s: S) => (a, s) }
    override def joinWith[A, B, Z](fa: State[S, A], fb: State[S, B])(f: (A, B) => Z): State[S, Z] = { (s0: S) =>
      val (a, s1) = fa(s0)
      val (b, s2) = fb(s1)
      val z = f(a, b)
      (z, s2)
    }
    override def sequence[A, Z](fas: Seq[State[S, A]])(f: Seq[A] => Z): State[S, Z] = { (s0: S) =>
      val as = Seq.newBuilder[A]
      var s = s0
      fas.foreach { fa =>
        val (a, t) = fa(s)
        as += a
        s = t
      }
      (f(as.result), s)
    }
    override def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = { (s0: S) =>
      val (a, s1) = fa(s0)
      val fb = f(a)
      fb(s1)
    }
  }

  class EitherMonad[E] extends Monad[({type L[A] = Either[E, A]})#L] {
    override def fmap[A, B](fa: Either[E, A])(f: A => B): Either[E, B] =
      fa match {
        case l@Left(_) => l.asInstanceOf[Either[E, B]]
        case Right(a) => Right(f(a))
      }
    override def pure[A](a: A): Either[E, A] = Right(a)
    override def joinWith[A, B, Z](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => Z): Either[E, Z] =
      (fa, fb) match {
        case (l@Left(_), _) => l.asInstanceOf[Either[E, Z]]
        case (_, l@Left(e)) => l.asInstanceOf[Either[E, Z]]
        case (Right(a), Right(b)) => Right(f(a, b))
      }
    override def sequence[A, Z](fas: Seq[Either[E, A]])(f: Seq[A] => Z): Either[E, Z] = {
      val as = Seq.newBuilder[A]
      fas.foreach { fa =>
        fa match {
          case l@Left(_) => return l.asInstanceOf[Either[E, Z]]
          case Right(a) => as += a
        }
      }
      Right(f(as.result))
    }
    override def bind[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa match {
        case l@Left(_) => l.asInstanceOf[Either[E, B]]
        case Right(a) => f(a)
      }
  }

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

  type Lazy[X] = Reader[Unit, X]

  type ReaderT[R, F[_], X] = R => F[X]

  type LazyT[F[_], X] = ReaderT[Unit, F, X]

  //trait MonadTrans[T[_[_], _]] {
  //  def lift[M[_], A](monad: Monad[M])(m: M[A]): T[M, A]
  //}

  // ???
  //class ReaderTMonadTrans[R] extends MonadTrans[({type L[F[_], A] = ReaderT[R, F, A]})#L] {
  //  override def lift[M[_], A](monad: Monad[M])(m: M[A]): ReaderT[R, M, A] = { (_: R) => m }
  //}

  class ReaderTFunctor[R, F[_]](
    functor: Functor[F]
  ) extends Functor[({type L[A] = ReaderT[R, F, A]})#L] {
    override def fmap[A, B](fa: ReaderT[R, F, A])(f: A => B): ReaderT[R, F, B] = { (r: R) =>
      functor.fmap(fa(r))(f)
    }
  }

  class ReaderTApplicative[R, F[_]](
    applicative: Applicative[F]
  ) extends ReaderTFunctor[R, F](applicative) with Applicative[({type L[A] = ReaderT[R, F, A]})#L] {
    override def pure[A](a: A): ReaderT[R, F, A] = { (_: R) => applicative.pure(a) }
    override def joinWith[A, B, Z](fa: ReaderT[R, F, A], fb: ReaderT[R, F, B])(f: (A, B) => Z): ReaderT[R, F, Z] = { (r: R) =>
      val ga = fa(r)
      val gb = fb(r)
      applicative.joinWith(ga, gb)(f)
    }
    override def sequence[A, Z](fas: Seq[ReaderT[R, F, A]])(f: Seq[A] => Z): ReaderT[R, F, Z] = { (r: R) =>
      val gas = fas map { _(r) }
      applicative.sequence(gas)(f)
    }
  }

  class ReaderTMonad[R, F[_]](
    monad: Monad[F]
  ) extends ReaderTApplicative[R, F](monad) with Monad[({type L[A] = ReaderT[R, F, A]})#L] {
    override def bind[A, Z](fa: ReaderT[R, F, A])(f: A => ReaderT[R, F, Z]): ReaderT[R, F, Z] = { (r: R) =>
      val ga = fa(r)
      monad.bind(ga) { (a: A) => f(a)(r) }
    }
  }

  //class OpFunctor[F[_]](functor: Functor[F]) extends Functor[({type L[A] = Op[LazyT[F], A]})#L] {
  //  private[this] val fTransId = transId[F]
  //  private[this] val lazyTFunctor = new ReaderTFunctor[Unit, F](functor)
  //  override def fmap[A, B](fa: Op[LazyT[F], A])(f: A => B): Op[LazyT[F], B] = {
  //    FMap(fa, f)
  //  }
  //  //override def pure[A, B](a: A): Op[F, A] = Pure(monad.pure(a))
  //  //override def joinWith[A, B](fa: Op[F, A], fb: Op[F, B])(f: (A, B) => Z): Op[F, Z] =
  //  //  JoinWith(fa.interpret(monad), fb.interpret(monad), f)
  //  //override def bind[A, B](fa: Op[F, A])(f: A => Op[F, B]): Op[F, B] = Bind(fa, f)
  //}

  def id[A]: A => A = { (a: A) => a }
  def transId[F[_]]: NatTrans[F, F] = new NatTrans[F, F] { override def trans[A](fa: F[A]) = fa }

  def const[A, B](b: B): A => B = { (_: A) => b }

  type LensLike[F[_], S, T, A, B] = (A => F[B]) => (S => F[T])

  trait Traversal[S, T, A, B] {
    def traversal[F[_]](f: A => F[B], applicative: Applicative[F]): (S => F[T])

    final def traversalLike[F[_]](applicative: Applicative[F]): LensLike[F, S, T, A, B] = { f =>
      traversal(f, applicative)
    }
  }

  trait Lens[S, T, A, B] extends Traversal[S, T, A, B] {
    def lens[F[_]](f: A => F[B], functor: Functor[F]): (S => F[T])

    final override def traversal[F[_]](f: A => F[B], applicative: Applicative[F]): (S => F[T]) =
      lens(f, applicative)

    final def lensLike[F[_]](functor: Functor[F]): LensLike[F, S, T, A, B] = { f =>
      lens(f, functor)
    }
  }

  trait IdLens[A, B] extends Lens[A, B, A, B] {
    override def lens[F[_]](f: A => F[B], functor: Functor[F]): A => F[B] = { a => f(a) }
  }

  trait FirstLens[A, B, C] extends Lens[(A, B), (C, B), A, C] {
    override def lens[F[_]](f: A => F[C], functor: Functor[F]): ((A, B)) => F[(C, B)] = { case (a, b) =>
      functor.fmap(f(a)) { c => (c, b) }
    }
  }

  trait SecondLens[A, B, C] extends Lens[(A, B), (A, C), B, C] {
    override def lens[F[_]](f: B => F[C], functor: Functor[F]): ((A, B)) => F[(A, C)] = { case (a, b) =>
      functor.fmap(f(b)) { c => (a, c) }
    }
  }

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

  type Getting[R, A, B, C, D] = LensLike[({type L[X] = Const[R, X]})#L, A, B, C, D]
  type Setter[A, B, C, D] = LensLike[Identity, A, B, C, D]

  object Lenses {
    def view[A, B, C, D](getting: Getting[C, A, B, C, D]): A => C = {
      val g: (A => Const[C, B]) = getting(id[C])
      val h: (A => C) = { (a: A) => g(a) }
      h
    }

    def set[A, B, C, D](setter: Setter[A, B, C, D]): D => A => B = { (d: D) =>
      val g: (A => Identity[B]) = setter(const[C, D](d))
      g
    }
  }
}
