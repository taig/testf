# Changelog

## 0.1.4

_2020-03-27_

`0.1.3` could not be released due to a misconfiguration, use `0.1.4` instead.

## 0.1.3

_2020-03-26_

 * Upgrade to sbt-scalajs 1.0.1
 * Upgrade to sbt-houserules 0.1.10
 * Upgrade to sbt 1.3.8
 * Upgrade to cats 2.1.1
 * Upgrade to cats-effect 2.1.2
 * Upgrade to scalacheck 1.14.3
 * Upgrade to scala 2.13.1
 * Upgrade to scala 2.12.11
 * Upgrade docker image to openjdk:8u212-jdk-alpine3.9
 * Add silencer to suppress warnings

## 0.1.2

_2019-11-06_

* `@AutoTest` now requires to extend from a `Discovery` trait
* Don't interpret auto discovered tests and instead abstract over F
* Add `Timer[IO]` instance to `IOTestApp`
* Rename `TestApp` to `IOTestApp`
* Add `Test.evalMap`

## 0.1.1

_2019-10-28_

 * Add `@AutoTest` support for classes
 * Add more `Interpreter` instances
 * Add test name to `AutoTestDiscovery#all`
 * Make `AutoTestDiscovery#auto` `protected`
 * Add `@AutoTest` support for complex class shapes
 * Add possibility to add additional tests to `AutoTestDiscovery`

## 0.1.0

_2019-10-22_

 * Going from `Test[F[_]]` to `Test[F[_], A]` once again
 * Introduce separate `runner-sbt` module
 * Change `@AutoTest` to pick up all terms in the `object`
 * `Interpreter` is no longer hard-coded to `IO`

## 0.0.8

_2019-05-22_

 * Everything is just a `Test[F[_]]`, Lists and Suites go away
 * Upgrade to sbt-ayendo-houserules 0.1.1

## 0.0.7

_2019-03-02_

 * Remove naming parameters from laws and checkers to favor labels instead
 * Automatically generate test labels in @AutoTestF

## 0.0.6

_2019-03-02_

 * Remove overloaded builders and add `@@` helper for labels

## 0.0.5

_2019-02-28_

 * Major simplification by removing `F[_]` from the Test ADT
 * Improve handling of failures in `F`
 * Properly handle and report test suite initialization errors

## 0.0.4

_2019-02-15_

 * Fix IndexOutOfBoundsException in AutoTestF on unrelated fields

## 0.0.3

_2019-02-15_

 * [#4] AutoTestF should maintain parents and ignore defs with arguments
 * [#5] AutoTestF should only pick up `Test[_, Unit]`

## 0.0.2

_2019-02-14_

 * [#3] Add `AutoTestF` macro
 * [#2] Add test execution time measurement
 * [#1] Migrate from `Test[A]` to `Test[F[_], A]`
 * Fix erroneous test summary report

## 0.0.1

_2019-02-01_

 * Initial release
