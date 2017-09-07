package xdean.jex.extra.function;

@FunctionalInterface
public interface SupplierThrow<V, T extends Exception> {
  V get() throws T;
}