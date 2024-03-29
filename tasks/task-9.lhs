% Задание 9. Монада списка. Эффект множественности

Требуется написать функцию `change`:

> change :: Int -> [Int] -> [[Int]]
> change n coins = undefined

которая разбивает переданную ей положительную сумму денег `n`
на монеты достоинств из списка `coins` всеми возможными способами.

Например, если `coins = [2,3,7]`:

```haskell
GHCI> change 7 [2,3,7]
[[2,2,3],[2,3,2],[3,2,2],[7]]
```

__Примечания__

* Функция `change` должна быть получена с помощью методов класса типов
`Monad`, использованных явно или неявно (через `do`-нотацию).
* Порядок монет в каждом разбиении имеет значение, то есть наборы
`[2,2,3]` и `[2,3,2]` -- различаются.
* _Предполагается заранее_, что `coins` содержит только уникальные
  положительные целые числа. Проверки `coins` на крайние случаи в коде
  _не нужны_.
