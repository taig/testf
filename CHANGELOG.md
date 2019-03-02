# CHANGELOG

## 0.0.6

_2019-03-02_

 * Remove overloaded builders and add `@@` helper for labels

## 0.0.5

_2019-02-28_

 * Major simplification by removing F[_] from the Test ADT
 * Improve handling of failures in F
 * Properly handle and report test suite initialization errors

## 0.0.4

_2019-02-15_

 * Fix IndexOutOfBoundsException in AutoTestF on unrelated fields

## 0.0.3

_2019-02-15_

 * [#4] AutoTestF should maintain parents and ignore defs with arguments
 * [#5] AutoTestF should only pick up Test[_, Unit]

## 0.0.2

_2019-02-14_

 * [#3] Add AutoTestF macro
 * [#2] Add test execution time measurement
 * [#1] Migrate from Test[A] to Test[F[_], A]
 * Fix erroneous test summary report

## 0.0.1

_2019-02-01_

 * Initial release