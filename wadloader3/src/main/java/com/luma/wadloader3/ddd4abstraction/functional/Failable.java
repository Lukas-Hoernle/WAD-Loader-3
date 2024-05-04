package com.luma.wadloader3.ddd4abstraction.functional;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * Class representing a failable computation returning a result. This is an alternative to exception handling forcing
 * the programmer to always think about possible failure of the underling computation.
 *
 * @param <T> the type of the return value.
 */
public sealed interface Failable<T> {
    static <T> Failable<T> fromOptional(Optional<T> optional, String error) {
        return optional.<Failable<T>>map(Success::new).orElse(Failure.of(error));
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

    default T getSuccess() {
        return switch (this) {
            case Success<T> success -> success.value;
            case Failure<T> ignored -> throw new RuntimeException("getSuccess() called on Failure");
        };
    }

    default List<String> getFailure() {
        return switch (this) {
            case Success<T> ignored -> List.of();
            case Failure<T> failure -> failure.error;
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
            case Failure<T> failure -> switch (other) {
                case Success<T2> ignore -> failure.coerce();
                case Failure<T2> failure2 ->
                        new Failure<>(Stream.of(failure, failure2).map(Failure::error).flatMap(List::stream).toList());
            };
        };
    }

    default Failable<T> join(Failable<Failable<T>> nested) {
        return switch (nested) {
            case Success<Failable<T>> success -> success.value;
            case Failure<Failable<T>> failure -> failure.coerce();
        };
    }

    default <R> Failable<R> chain(Supplier<R> f) {
        return switch (this) {
            case Success<T> ignored -> new Success<>(f.get());
            case Failure<T> failure -> failure.coerce();
        };
    }

    default T orElse(T alternative) {
        return switch (this) {
            case Success<T> (T value) -> value;
            case Failure<T> ignored -> alternative;
        };
    }

    record Failure<T>(List<String> error) implements Failable<T> {
        public static <T> Failure<T> of(String error) {
            return new Failure<>(List.of(error));
        }
        <R> Failure<R> coerce() {
            return (Failure<R>) this;
        }
    }


}
