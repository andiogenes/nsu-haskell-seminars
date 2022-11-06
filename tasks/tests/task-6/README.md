Система тестирования для задания №6
===================================

How to run
----------

### Через скрипт:

* `.\run-tests.bat` на Windows
* `./run-tests.sh` на Linux/macOS

### Через GHCi:

```shell
ghci Tests.hs

*Tests> test_All
Test prop_PrimesArePrime succeed.
Test prop_PrimesUpToN succeed.
Test prop_PrimesAreOrdered succeed.
Test prop_UnionIsTotal succeed.
Test prop_UnionContainsUniques succeed.
```

Задание
-------

В файле `Task6.hs` написать реализации для функций `(<>)`, `unique`,
`primesUpToN` в соответствии с требованиями, описанными в
`task-6.pdf`.

Прохождение тестов является необходимым, но не достаточным, условием
сдачи задания.