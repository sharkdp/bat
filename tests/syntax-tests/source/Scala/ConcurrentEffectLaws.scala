/*
 * Copyright (c) 2017-2019 The Typelevel Cats-effect Project Developers
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats
package effect
package laws

import cats.effect.concurrent.Deferred
import cats.syntax.all._
import cats.laws._

import scala.concurrent.Promise

trait ConcurrentEffectLaws[F[_]] extends ConcurrentLaws[F] with EffectLaws[F] {
  implicit def F: ConcurrentEffect[F]

  def runAsyncRunCancelableCoherence[A](fa: F[A]) = {
    val fa1 = IO.async[A] { cb =>
      F.runAsync(fa)(r => IO(cb(r))).unsafeRunSync()
    }
    val fa2 = IO.cancelable[A] { cb =>
      F.toIO(F.runCancelable(fa)(r => IO(cb(r))).unsafeRunSync())
    }
    fa1 <-> fa2
  }

  def runCancelableIsSynchronous[A] = {
    val lh = Deferred.uncancelable[F, Unit].flatMap { latch =>
      val spawned = Promise[Unit]()
      // Never ending task
      val ff = F.cancelable[A] { _ =>
        spawned.success(()); latch.complete(())
      }
      // Execute, then cancel
      val token = F.delay(F.runCancelable(ff)(_ => IO.unit).unsafeRunSync()).flatMap { cancel =>
        // Waiting for the task to start before cancelling it
        Async.fromFuture(F.pure(spawned.future)) >> cancel
      }
      F.liftIO(F.runAsync(token)(_ => IO.unit).toIO) *> latch.get
    }
    lh <-> F.unit
  }

  def runCancelableStartCancelCoherence[A](a: A) = {
    // Cancellation via runCancelable
    val f1: F[A] = for {
      effect1 <- Deferred.uncancelable[F, A]
      latch <- F.delay(Promise[Unit]())
      never = F.cancelable[A] { _ =>
        latch.success(()); effect1.complete(a)
      }
      cancel <- F.liftIO(F.runCancelable(never)(_ => IO.unit).toIO)
      // Waiting for the task to start before cancelling it
      _ <- Async.fromFuture(F.pure(latch.future)) // TODO get rid of this, IO, and Future here
      _ <- cancel
      result <- effect1.get
    } yield result

    // Cancellation via start.flatMap(_.cancel)
    val f2: F[A] = for {
      effect2 <- Deferred.uncancelable[F, A]
      // Using a latch to ensure that the task started
      latch <- Deferred.uncancelable[F, Unit]
      never = F.bracket(latch.complete(()))(_ => F.never[Unit])(_ => effect2.complete(a))
      fiber <- F.start(never)
      // Waiting for the task to start before cancelling it
      _ <- latch.get
      _ <- F.start(fiber.cancel)
      result <- effect2.get
    } yield result

    f1 <-> f2
  }

  def toIORunCancelableConsistency[A](fa: F[A]) =
    ConcurrentEffect.toIOFromRunCancelable(fa) <-> F.toIO(fa)
}

object ConcurrentEffectLaws {
  def apply[F[_]](implicit F0: ConcurrentEffect[F], contextShift0: ContextShift[F]): ConcurrentEffectLaws[F] =
    new ConcurrentEffectLaws[F] {
      val F = F0
      val contextShift = contextShift0
    }
}
