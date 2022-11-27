% Задание 8. Монада Maybe. Эффект частичности

Требуется реализовать функцию `fSafe :: Double -> Maybe Double`,
соответствующую следующему выражению:

$$f(x) = \frac{tg (ln^e x) + \frac{1}{2~cos~x}}{x^2 + 2x - 10}.$$

Функция должна возвращать `Nothing`, если $f(x)$ не определена в точке
$x$, в других случаях возвращать `Just` $f(x)$.

Для решения основной задачи предлагается реализовать "безопасные"
функции-обёртки над используемыми тригонометрическими,
логарифмическими функциями и оператором деления. Эти функции должны
возвращать `Nothing`, если исходная функция не определена при заданных
параметрах, или вычисленное значение, запакованное в `Maybe` в
остальных случаях.

Функция `fSafe` должна быть получена с помощью методов класса типов
`Monad`, использованных явно или неявно (через `do`-нотацию).
