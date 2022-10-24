Система тестирования для задания №5
===================================

How to run
----------

## Через скрипт:

* `.\run-tests.bat` на Windows
* `./run-tests.sh` на Linux/macOS

## Через GHCi:

```shell
ghci Task5.hs

*Task5> test_All
Suite "test_toSeries":
Tests: 0 passed, 1 failed

Suite "test_encodeSeries":
Tests: 0 passed, 1 failed

Suite "test_writeCode":
Tests: 0 passed, 1 failed

Suite "test_makeAssoc":
Tests: 1 passed, 3 failed
```

Задание
-------

В файле `Task5.hs` написать реализации для функций `toSeries`,
`encodeSeries`, `writeCode`, `makeAssoc` в соответствии с
требованиями, описанными в `task-5.pdf`.

Прохождение тестов является необходимым, но не достаточным, условием
сдачи задания.