package com.luma.wadloader3.ddd4abstraction.functional;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Class representing a failable computation returning a result. This is an alternative to exception handling forcing
 * the programmer to always think about possible failure of the underling computation.
 *
 * @param <T> the type of the return value.
 */
public sealed interface Failable<T> {
    record Failure<T>(String error) implements Failable<T> {
        <R> Failure<R> coerce() {
            return (Failure<R>) this;
        }
    }

    record Success<T>(T value) implements Failable<T> {
    }

    default boolean isError() {
        return this instanceof Failable.Failure<T>;
    }

    default boolean isSuccess() {
        return this instanceof Failable.Success<T>;
    }

    /**
     * @param f   function mapping the value of {@link Success}
     * @param <R> the new type of the Failable
     * @return if this element is of type {@link Failure} nothing happens, else the value received by applying f. This
     * might result in {@link Failure} or {@link Success}.
     */
    default <R> Failable<R> apply(Function<T, Failable<R>> f) {
        return switch (this) {
            case Success<T> success -> f.apply(success.value);
            case Failure<T> failure -> failure.coerce();
        };
    }

    /**
     * @param f   function mapping the value of {@link Success}
     * @param <R> the new type of the Failable
     * @return the {@link Success} value mapped by f or the {@link Failure} value previously present.
     */
    default <R> Failable<R> map(Function<T, R> f) {
        return this.apply(f.andThen(Success::new));
    }

    default Optional<T> getSuccess() {
        return switch (this) {
            case Success<T> success -> Optional.of(success.value);
            case Failure<T> ignored -> Optional.empty();
        };
    }

    default Optional<String> getFailure() {
        return switch (this) {
            case Success<T> ignored -> Optional.empty();
            case Failure<T> failure -> Optional.of(failure.error);
        };
    }

    /**
     * @param f     function used to combine the values held this and other
     * @param other {@link Failable} holding a value
     * @param <T2>  the type of Failable other
     * @param <R>   the returntype of f
     * @return returns the result of applying f to this and other or any {@link Failure} in this or other.
     */
    default <T2, R> Failable<R> combine(Failable<T2> other, BiFunction<T, T2, R> f) {
        return switch (this) {
            case Success<T> success -> other.map(((T2 v) -> f.apply(success.value, v)));
            case Failure<T> failure -> failure.coerce();
        };
    }

    default Failable<T> join(Failable<Failable<T>> nested) {
        return switch (nested) {
            case Success<Failable<T>> success -> success.value;
            case Failure<Failable<T>> failure -> failure.coerce();
        };
    }

    default T orElse(T alternative) {
        return switch (this) {
            case Success<T> (T value) -> value;
            case Failure<T> ignored -> alternative;
        };
    }

    static <T> Failable<T> fromOptional(Optional<T> optional, String error) {
        return optional.<Failable<T>>map(Success::new).orElse(new Failure<>(error));
    }
}
