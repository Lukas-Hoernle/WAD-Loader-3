package com.luma.wadloader3.ddd4abstraction.functional;

import java.util.Optional;
import java.util.function.Function;

public sealed interface Either<L, R> {
    record Left<L, R>(L left) implements Either<L, R> {
    }

    record Right<L, R>(R right) implements Either<L, R> {
    }

    default boolean isLeft() {
        return this instanceof Either.Left<L, R>;
    }

    default boolean isRight() {
        return this instanceof Either.Left<L, R>;
    }

    default <L2, R2> Either<L2, R2> map(Function<Either<L, R>, Either<L2, R2>> f) {
        return f.apply(this);
    }

    default Optional<L> getLeft() {
        return this instanceof Either.Left<L, R> left ? Optional.of(left.left) : Optional.empty();
    }

    default Optional<R> getRight() {
        return this instanceof Either.Right<L, R> right ? Optional.of(right.right) : Optional.empty();
    }

    default <R2> Either<L, R2> mapRight(Function<R, R2> f) {
        return switch (this) {
            case Right(R right) -> new Right<>(f.apply(right));
            //this cast is safe because type Left does not care about type R or R2
            case Left<L,R> left -> (Either<L, R2>) left;
        };
    }

}

