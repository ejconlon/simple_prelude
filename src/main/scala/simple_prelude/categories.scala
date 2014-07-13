package simple_prelude

import scala.language.higherKinds

object FunctionCategory extends Category[Function1] {
  override def id[A]: A => A = { (a: A) => a }
  override def andThen[A, B, C](x: A => B, y: B => C): A => C = x andThen y
}

class KleisliCategory[M[_]](monad: Monad[M]) extends Category[({type L[A, B] = Kleisli[M, A, B]})#L] {
  override def id[A]: Kleisli[M, A, A] = { (a: A) => monad.pure(a) }
  override def andThen[A, B, C](x: Kleisli[M, A, B], y: Kleisli[M, B, C]): Kleisli[M, A, C] = { (a: A) =>
    monad.bind(x(a))(y)
  }
}

class CoKleisliCategory[W[_]](comonad: Comonad[W]) extends Category[({type L[A, B] = CoKleisli[W, A, B]})#L] {
  override def id[A]: CoKleisli[W, A, A] = { (wa: W[A]) => comonad.extract(wa) }
  override def andThen[A, B, C](x: CoKleisli[W, A, B], y: CoKleisli[W, B, C]): CoKleisli[W, A, C] = { (wa: W[A]) =>
    y(comonad.extend(wa)(x))
  }
}
