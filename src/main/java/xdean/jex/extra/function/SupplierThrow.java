package xdean.jex.extra.function;

@FunctionalInterface
public interface SupplierThrow<V, T extends Throwable> {
  V get() throws T;
}