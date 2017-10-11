# Java-EX
Java Common Extension

[![Build Status](https://travis-ci.org/XDean/Java-EX.svg?branch=master)](https://travis-ci.org/XDean/Java-EX)

# Features
## Lang
- Finalize support. Attach clean up tasks on any object. Free from `Object.finalize`.
- Size of object. Measure shallow and retained size of object or class by java code which is useful when debug.

## Reflect
- Add or change annotations on Class/Method/Field at runtime.
- Get explicit generic type of class or interface.
- Get caller (any depth in stack) information
- Get more information of class: getAll(Field/Method/Interface/SuperClass)s, getRoot(Field/Method)

## Collection
- `Either`. Left or right, that is a question.
- `Pair`. Left and right, that is no question.
- `IntList`. Operate int array easier. (not `java.util.List`)
- `TreeNode`. A powerful tree implementation. (not dependable yet)
- `Traverse`. Traverse any tree structure. Provide default PreOrder/PostOrder/BreadthFirst traverser.

## Easy Programming / Function Programming
- Functions with throws.
- Ignore checked exception by `uncheck`.
- Change throw exception to return it.
- Cache everywhere like dynamic field.
- Let everything lambda and let lambda one line:
	- `Try`
	- `If`
	- `TaskUtil`

## RxJava Extension
- From java functions to Rx functions.
- From `Observable`/`Flowable` to `Iterator`.
- `RandomOperator`. Easy to shuffle.

## Other many utilities
See the code!
