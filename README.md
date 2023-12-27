# Slang-Emulator
Выполнил: Баранов Вячеслав Григорьевич, 335185

Вариант: miranda | risc | harv | hw | instr | struct | stream | port | pstr | prob5
## Язык программирования
```Slang``` &ndash; это упрощенный функциональный язык программирования, похожий на Miranda и имеющий небольшие синтаксические отличия.
Так как язык содержит немало конструкций, я решил привести его описание в виде БНФ с синтаксическим сахаром

```Haskell
Program ::= [FunctionDefinition]
FunctionDefinition ::= FunctionName "(" DefinitionArgs ")" Expr
DefinitionArgs ::= (Variable)
Expr ::= "(" Expr ")" | FunctionCall | LetExpr | IfThenElseExpr | BinOp Variable Variable | UnOp Variable | Variable | Primitive
FunctionCall ::= FunctionName CallArgs
LetExpr ::= "let" [Expr] "in" Expr
IfThenElseExpr ::= "if" Expr "then" [Expr] | "if" Expr "then" [Expr] "else" [Expr]
BinOp ::= + | - | * | / | == | > | < | /= | ++
UnOp ::= - | not | head | tail
CallArgs ::= (Expr)
FunctionName ::= Name
Variable ::= Name
Name ::= UTF8-String
Primitive :: = Int | String | List | Bool
```
где
```Haskell
[T] - список элементов типа T, (T) - кортеж элементов типа T
```
Подробнее c синтаксисом языка можно ознакомиться можно ![здесь]()

Cтоит отметить, что язык не поддерживает pattern-matching, а работа со списками осуществляется с помощью функций из стандартной библиотеки языка. Такое решение было принято для того, чтобы не переусложнять логику компилятора.

## Организация памяти
Модель памяти процессора:
1. Память команд. Машинное слово &ndash; 32 бита. Линейное адресное пространство. Реализуется как Map* Int Instruction**
2. Память данных. Машинное слово &ndash; 32 бита. Линейное адресное пространсвто. Реализуется как Map Int Int
3. Порт ввода. Машинное слово &ndash; 32 бита. В него умещаются 4 utf-8 символа. Линейное адресное пространство. Реализуется как Map Int Int
4. Порт вывода. Машинное слово &ndash; 32 бита. В него умещаются 4 utf-8 символа. Линейное адресное пространство. Реализуется как Map Int Int

*Я предпочел Data.Map обычным спискам Haskell ввиду удобства (и как не странно эффективности).

**Instruction &ndash; высокоуровневая структура, см. ![тут](src/ISA.hs)

Таким образом, реализуется усложение ```char[4]```
## Система команд (ISA)
Система команд содержит машинные слова фиксированного размера &ndash 32 бита. Существует 3 типа инструкций:
![Формат инструкций](images/instruction-formats.png)
1. I-type для инструкций типа rd = f(rs1, rs2) и условных переходов (см. набор инструкций)
2. II-type для инструкций типа rd = f(rs1, const)
3. III-type для безусловного перехода

## Набор инструкций
![Список доступных инструкций](images/supported-instructions.png)

Также для удобства существует набор псевдоинструкций, таких как ```push rd``` и ```pop rd```, которые обрабатываются препроцессором и заменяются на реальные инструкции.
## Кодирование инструкций
Машинный код не сериализуется, а преобрауется в структуру данных (см. TBD)
## Транслятор
TBD* Можно написать транслятор для ассемблера
## Модель процессора
### DataPath
### Control Unit
## Тестирование
В качестве тестов было использовано:
1. hello world
2. cat
3. prob5
Тестирование проводилось при помощи golden tests. Примеры тестов можно найти в папке golden

Для настройки CI я использовал github actions:
```jaml
jobs:
  build-and-test:
    runs-on: ubuntu-latest
    container: haskell:9.4.8-buster
    steps:
      - name: Checkout
        uses: actions/checkout@v3
      - run: cabal update
      - uses: haskell-actions/hlint-setup@v2
        with:
          version: '3.1.6'
      - name: Lint
        uses: haskell-actions/hlint-run@v2
        with:
          path: '["src/", "test/"]'
          fail-on: warning
      - name: Build
        run: cabal build
      - name: Test
        run: cabal run Emulator-test
```
где:
1. ```hlint``` -- линтер для Haskell
1. Build -- сборка проекта при помощи ```cabal```
1. Test -- запуск тестов

Для удобства я также настроил пре-коммит хуки с использованием форматера, запуском линтера и тестов. Ознакомитьс можно [тут](.pre-commit-config.yaml)

Журнал работы процессора на примере ```cat```:

TBD
Пример проверки исходного кода:

TBD
## Алгоритмы
TBD