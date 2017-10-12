package xdean.jex.extra.rx2.nullable.source;

/**
 *
 * To avoid write duplicate code in NullableFlowable, NullableObservable, NullableObservableFlowable.
 * But java don't allow add generic on generic type (TypeVariable isn't implement GenericDeclaration).
 *
 * So I do some cheat. I use Generic to let Flowable, Observable and ObservableFlowable be subclass of
 * same type. And the NullableSource returns Generic type. So now I can write common methods in NullableSource.
 * Subclass just need declare return types on its generic.
 *
 * Unchecked warning only in rx2.nullable.source package. Usage is type safe.
 *
 * @author XDean
 *
 */
