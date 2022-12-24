% Задание 10. Монада IO. Простой ввод-вывод в Haskell

Предисловие
-----------

В школе Чародейства и волшебства Хогвартс проходит модернизация -- все
В одушевлённые волшебные предметы, призраков, эльфов отправляют в бессрочный
оплачиваемый отпуск и заменяют на волшебные автоматизированные системы.

Модернизации подверглись и гостиные факультетов -- например, гриффиндорцы теперь
попадают домой не после оглашения пароля портрету Полной Дамы, а после того, как
угадают секретное число, загаданное волшебным вычислителем.

Похожую систему пропусков решили установить и в подземельях Слизерина. Однако,
студентам Слизерина не нравится долго стоять в сыром подвале, пытаясь отгадать
число. К счастью, в 2022 году у всех молодых волшебников в кармане лежит волшебный
программируемый калькулятор, в который можно загружать программы на волшебном
языке программирования Haskell. Поэтому, cтуденты Слизерина решили написать программу,
которая будет автоматически угадывать число, задуманное пропускной системой.

Правда, слизеринцы не разобрались с вводом-выводом (они больше любят чистые функции),
поэтому программу предложено написать вам.

Постановка задачи
-----------------

Написать программу, которая:

1. загадывает случайное число $u$ от $a$ до $b$ __включительно__ и печатает его на экран;
2. предлагает ввести пользователю один из трёх символов:
   * `=`, означающий, что число $u$ совпадает с загаданным пользователем;
   * `>`, означающий, что число $u$ больше, чем загаданное пользователем;
   * `<`, означающий, что число $u$ меньше, чем загаданное пользователем;
3. если пользователь ввёл `=`, печатает сообщение об успешном вводе и завершает исполнение программы;
4. если пользователь ввёл `>`, то начинает процедуру заново, но случайное число выбирается на диапазоне $[u, b]$;
5. если пользователь ввёл `<`, то начинает процедуру заново, но случайное число выбирается на диапазоне $[a, u]$.

* На первой итерации $a = 1, b = 100$;
* если на очередной итерации $a$ становится равным $b$, но пользователь говорит, что число,
  предложенное программой, отличается от загаданного, программа должна вывести сообщение о
  недопустимом вводе и начать исполнение заново, считая $a = 1, b = 100$.

Пример возможного взаимодействия программы и пользователя
---------------------------------------------------------
```
Программа> Загадайте число от 1 до 100 (включительно), а я попробую его отгадать.
Программа> Ваше число - 87?
Пользователь> abcdef
Программа> Что-то не сходится... Попробуйте еще раз!
Пользователь> <
Программа> Ваше число - 99?
Пользователь> <
Программа> Ваше число - 100?
Пользователь> >
Программа> Похоже, вы меня обманываете... Давайте еще раз?
Программа> Загадайте число от 1 до 100 (включительно), а я попробую его отгадать.
Программа> Ваше число - 45?
Пользователь> =
Программа> Ура, я так и знал!
```