# Slang-Emulator
Выполнил: Баранов Вячеслав Григорьевич, 335185

Вариант: miranda | risc | harv | hw | instr | struct | stream | port | pstr | prob5 | pipeline
## Язык программирования
```Slang``` &ndash; это упрощенный функциональный язык программирования, похожий на Miranda и имеющий небольшие синтаксические отличия.
Так как язык содержит немало конструкций, я решил привести его описание в виде БНФ с синтаксическим сахаром

```Haskell
Program ::= [FunctionDefinition]
FunctionDefinition ::= FunctionName "(" DefinitionArgs ")" "=" Expr
VariableDefinition ::= VariableName "=" Expr
DefinitionArgs ::= (Variable)
Expr ::= "(" Expr ")" | FunctionCall | LetExpr | IfThenElseExpr | BinOp Expr Expr | UnOp Expr | Primitive
FunctionCall ::= FunctionName CallArgs
LetExpr ::= "Let" [VariableDefinition] "In" VariableDefinition
IfThenElseExpr ::= "If" Expr "Then" [Expr] | "If" Expr "Then" [Expr] "Else" [Expr]
BinOp ::= + | - | * | / | == | > | < | /= | ++
UnOp ::= - | not | head | tail
CallArgs ::= (Expr)
FunctionName ::= Name
VariableName ::= Name
Variable ::= Name
Name ::= UTF8-String
Primitive ::= Int | String | List | Bool
```
где
```Haskell
[T] - список элементов типа T, (T) - кортеж элементов типа T
```

Cтоит отметить, что язык не поддерживает pattern-matching, а работа со списками осуществляется с помощью функций из стандартной библиотеки языка. Такое решение было принято для того, чтобы не переусложнять логику компилятора.

## Организация памяти
Модель памяти процессора:
1. Память команд. Машинное слово &ndash; 64 бита. Линейное адресное пространство. Реализуется словарем высокоруровневых структур (см. [Instruction](src/ISA.hs#L103))
2. Память данных. Машинное слово &ndash; 64 бита. Линейное адресное пространсвто. Реализуется словарем
3. Порт ввода. Машинное слово &ndash; 64 бита. Линейное адресное пространство. Реализуется словарем
4. Порт вывода. Машинное слово &ndash; 64 бита. Линейное адресное пространство. Реализуется словарем

Я предпочел Data.Map обычным спискам Haskell ввиду удобства (и как не странно эффективности).

## Система команд (ISA)
Система команд содержит машинные слова фиксированного размера &ndash 64 бита. Существует 4 типа инструкций:
![Формат инструкций](images/instruction-formats.png)

## Набор инструкций

| №  | Opcode | Инструкция | Тип машинного слова | Тип инструкции | Описание                       |
|----|--------|------------|---------------------|----------------|--------------------------------|
| 1  | 0      | Add        | R                   | R-R            | rd <- @rs1 + @rs2              |
| 2  | 1      | Sub        | R                   | R-R            | rd <- @rs1 - @rs2              |
| 3  | 2      | Mul        | R                   | R-R            | rd <- @rs1 * @rs2              |
| 4  | 3      | Div        | R                   | R-R            | rd <- @rs1 / @rs2              |
| 5  | 8      | Mod        | R                   | R-R            | rd <- @rs1 % @rs2              |
| 6  | 16     | AddI       | I                   | R-R            | rd <- @rs1 + imm               |
| 7  | 17     | SubI       | I                   | R-R            | rd <- @rs1 - imm               |
| 8  | 18     | MulI       | I                   | R-R            | rd <- @rs1 * imm               |
| 9  | 19     | DivI       | I                   | R-R            | rd <- @rs1 / imm               |
| 10 | 24     | ModI       | I                   | R-R            | rd <- @rs1 % imm               |
| 11 | 20     | LWM        | I                   | R-M            | rd <- @data[@rs1+imm]          |
| 12 | 21     | SWM        | I                   | R-M            | data[@rs1+imm] <- @rd          |
| 13 | 22     | LWI        | I                   | M-M            | data[@rs1+imm] <- in[@inPtr]   |
| 14 | 23     | SWO        | I                   | M-M            | out[@outPtr] <- data[@rs1+imm] |
| 15 | 4      | JE         | B                   | Branch         | if(@rs1==@rs2) pc <- pc + imm  |
| 16 | 5      | JG         | B                   | Branch         | if(@rs1>@rs2) pc <- pc + imm   |
| 17 | 6      | JNE        | B                   | Branch         | if(@rs1<>@rs2) pc <- pc + imm  |
| 18 | 7      | JL         | B                   | Branch         | if(@rs1<@rs2) pc <- pc + imm   |
| 19 | 31     | JMP        | J                   | Jump           | pc <- @rd + imm                |
| 20 | 25     | SavePC     | R                   | R-R            | ra <- pc                       |

### Псевдоинстркуции
| №  | Инструкция  | Тип машинного слова | Тип инструкции | Описание                               |
|----|-------------|---------------------|----------------|--------------------------------        |
| 1  | JEL         | B                   | Branch         | if(@rs1==@rs2) pc <- @label            |
| 2  | JGL         | B                   | Branch         | if(@rs1>@rs2) pc <- @label             |
| 3  | JNEL        | B                   | Branch         | if(@rs1<>@rs2) pc <- @label            |
| 4  | JLL         | B                   | Branch         | if(@rs1<@rs2) pc <- @label             |
| 5  | JMPL        | J                   | Jump           | pc <- @label                           |
| 6  | PUSH        | -                   | -              | swm rs sp 0, subI sp sp 1              |
| 7  | POP         | -                   | -              | addI sp sp 1, lwm rd sp 0              |
| 8  | СALL        | -                   | -              | push ra, savePC, jump @label, pop ra   |

## Кодирование инструкций
Машинный код используется в виде высокоуровневой структуры данных (см. [Instruction](src/ISA.hs#L103))
## Транслятор
TBD* Можно написать транслятор для ассемблера
## Модель процессора
### DataPath
![DataPath](images/datapath.png)
### Control Unit
![ControlUnit](images/control_unit.png)
## Тестирование
В качестве тестов было использовано:
1. hello
2. cat
3. hello user
4. prob5

Тестирование проводилось при помощи golden tests и [unit tests для транслятора](test/unit/EmulatorTest.hs). Примеры тестов можно найти в папке golden

Для настройки CI я использовал github actions:
```jaml
jobs:
  lint:
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
          fail-on: error
  build-and-test:
    runs-on: ubuntu-latest
    container: haskell:9.4.8-buster
    steps:
      - name: Checkout
        uses: actions/checkout@v3
      - run: cabal update
      - name: Build
        run: cabal build Compiler-Release-exe Emulator-Release-exe Unit-tests-Release Golden-tests-Release
      - name: Unit-tests
        run: cabal run Unit-tests-Release
      - name: Golden-tests
        run: cabal run Golden-tests-Release
```
где:
1. ```hlint``` -- линтер для Haskell
2. Build -- сборка проекта при помощи ```cabal```
3. Test -- запуск тестов

Для удобства я также настроил пре-коммит хуки с использованием форматера, запуском линтера и тестов. Ознакомиться можно [тут](.pre-commit-config.yaml)

Журнал работы процессора на примере ```cat```:
```yaml
pc: 0      | instruction: swm ra sp 0       | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 0 , sp: 4096, tr: 0 | rin : 0, rout: 0
pc: 8      | instruction: subI sp sp 1      | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 0 , sp: 4096, tr: 0 | rin : 0, rout: 0
pc: 16     | instruction: savePC            | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 0 , sp: 4095, tr: 0 | rin : 0, rout: 0
pc: 24     | instruction: jump zero 56      | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4095, tr: 0 | rin : 0, rout: 0
pc: 56     | instruction: nop               | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4095, tr: 0 | rin : 0, rout: 0
pc: 64     | instruction: addI t0 zero 0    | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4095, tr: 0 | rin : 0, rout: 0
pc: 72     | instruction: add t1 a0 zero    | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4095, tr: 0 | rin : 0, rout: 0
pc: 80     | instruction: swm t0 sp 0       | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4095, tr: 0 | rin : 0, rout: 0
pc: 88     | instruction: subI sp sp 1      | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4095, tr: 0 | rin : 0, rout: 0
pc: 96     | instruction: add a0 t0 zero    | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4094, tr: 0 | rin : 0, rout: 0
pc: 104    | instruction: swm ra sp 0       | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4094, tr: 0 | rin : 0, rout: 0
pc: 112    | instruction: subI sp sp 1      | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4094, tr: 0 | rin : 0, rout: 0
pc: 120    | instruction: savePC            | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 16, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 128    | instruction: jump zero 288     | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 288    | instruction: nop               | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 296    | instruction: lwm t0 a0 0       | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 304    | instruction: nop               | t0: 13, t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 312    | instruction: je t0 zero 32     | t0: 13, t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 320    | instruction: addI a0 a0 1      | t0: 13, t1: 0 , t2: 0 , t3: 0 , a0: 0 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 328    | instruction: swo a0 0          | t0: 13, t1: 0 , t2: 0 , t3: 0 , a0: 1 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 0
pc: 336    | instruction: subI t0 t0 1      | t0: 13, t1: 0 , t2: 0 , t3: 0 , a0: 1 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 8
pc: 344    | instruction: jump zero 304     | t0: 12, t1: 0 , t2: 0 , t3: 0 , a0: 1 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 8
pc: 304    | instruction: nop               | t0: 12, t1: 0 , t2: 0 , t3: 0 , a0: 1 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 8
pc: 312    | instruction: je t0 zero 32     | t0: 12, t1: 0 , t2: 0 , t3: 0 , a0: 1 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 8
pc: 320    | instruction: addI a0 a0 1      | t0: 12, t1: 0 , t2: 0 , t3: 0 , a0: 1 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 8
pc: 328    | instruction: swo a0 0          | t0: 12, t1: 0 , t2: 0 , t3: 0 , a0: 2 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 8
pc: 336    | instruction: subI t0 t0 1      | t0: 12, t1: 0 , t2: 0 , t3: 0 , a0: 2 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 16
pc: 344    | instruction: jump zero 304     | t0: 11, t1: 0 , t2: 0 , t3: 0 , a0: 2 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 16
pc: 304    | instruction: nop               | t0: 11, t1: 0 , t2: 0 , t3: 0 , a0: 2 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 16
pc: 312    | instruction: je t0 zero 32     | t0: 11, t1: 0 , t2: 0 , t3: 0 , a0: 2 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 16
pc: 320    | instruction: addI a0 a0 1      | t0: 11, t1: 0 , t2: 0 , t3: 0 , a0: 2 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 16
pc: 328    | instruction: swo a0 0          | t0: 11, t1: 0 , t2: 0 , t3: 0 , a0: 3 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 16
pc: 336    | instruction: subI t0 t0 1      | t0: 11, t1: 0 , t2: 0 , t3: 0 , a0: 3 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 24
pc: 344    | instruction: jump zero 304     | t0: 10, t1: 0 , t2: 0 , t3: 0 , a0: 3 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 24
pc: 304    | instruction: nop               | t0: 10, t1: 0 , t2: 0 , t3: 0 , a0: 3 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 24
pc: 312    | instruction: je t0 zero 32     | t0: 10, t1: 0 , t2: 0 , t3: 0 , a0: 3 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 24
pc: 320    | instruction: addI a0 a0 1      | t0: 10, t1: 0 , t2: 0 , t3: 0 , a0: 3 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 24
pc: 328    | instruction: swo a0 0          | t0: 10, t1: 0 , t2: 0 , t3: 0 , a0: 4 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 24
pc: 336    | instruction: subI t0 t0 1      | t0: 10, t1: 0 , t2: 0 , t3: 0 , a0: 4 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 32
pc: 344    | instruction: jump zero 304     | t0: 9 , t1: 0 , t2: 0 , t3: 0 , a0: 4 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 32
pc: 304    | instruction: nop               | t0: 9 , t1: 0 , t2: 0 , t3: 0 , a0: 4 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 32
pc: 312    | instruction: je t0 zero 32     | t0: 9 , t1: 0 , t2: 0 , t3: 0 , a0: 4 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 32
pc: 320    | instruction: addI a0 a0 1      | t0: 9 , t1: 0 , t2: 0 , t3: 0 , a0: 4 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 32
pc: 328    | instruction: swo a0 0          | t0: 9 , t1: 0 , t2: 0 , t3: 0 , a0: 5 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 32
pc: 336    | instruction: subI t0 t0 1      | t0: 9 , t1: 0 , t2: 0 , t3: 0 , a0: 5 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 40
pc: 344    | instruction: jump zero 304     | t0: 8 , t1: 0 , t2: 0 , t3: 0 , a0: 5 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 40
pc: 304    | instruction: nop               | t0: 8 , t1: 0 , t2: 0 , t3: 0 , a0: 5 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 40
pc: 312    | instruction: je t0 zero 32     | t0: 8 , t1: 0 , t2: 0 , t3: 0 , a0: 5 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 40
pc: 320    | instruction: addI a0 a0 1      | t0: 8 , t1: 0 , t2: 0 , t3: 0 , a0: 5 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 40
pc: 328    | instruction: swo a0 0          | t0: 8 , t1: 0 , t2: 0 , t3: 0 , a0: 6 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 40
pc: 336    | instruction: subI t0 t0 1      | t0: 8 , t1: 0 , t2: 0 , t3: 0 , a0: 6 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 48
pc: 344    | instruction: jump zero 304     | t0: 7 , t1: 0 , t2: 0 , t3: 0 , a0: 6 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 48
pc: 304    | instruction: nop               | t0: 7 , t1: 0 , t2: 0 , t3: 0 , a0: 6 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 48
pc: 312    | instruction: je t0 zero 32     | t0: 7 , t1: 0 , t2: 0 , t3: 0 , a0: 6 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 48
pc: 320    | instruction: addI a0 a0 1      | t0: 7 , t1: 0 , t2: 0 , t3: 0 , a0: 6 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 48
pc: 328    | instruction: swo a0 0          | t0: 7 , t1: 0 , t2: 0 , t3: 0 , a0: 7 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 48
pc: 336    | instruction: subI t0 t0 1      | t0: 7 , t1: 0 , t2: 0 , t3: 0 , a0: 7 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 56
pc: 344    | instruction: jump zero 304     | t0: 6 , t1: 0 , t2: 0 , t3: 0 , a0: 7 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 56
pc: 304    | instruction: nop               | t0: 6 , t1: 0 , t2: 0 , t3: 0 , a0: 7 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 56
pc: 312    | instruction: je t0 zero 32     | t0: 6 , t1: 0 , t2: 0 , t3: 0 , a0: 7 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 56
pc: 320    | instruction: addI a0 a0 1      | t0: 6 , t1: 0 , t2: 0 , t3: 0 , a0: 7 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 56
pc: 328    | instruction: swo a0 0          | t0: 6 , t1: 0 , t2: 0 , t3: 0 , a0: 8 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 56
pc: 336    | instruction: subI t0 t0 1      | t0: 6 , t1: 0 , t2: 0 , t3: 0 , a0: 8 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 64
pc: 344    | instruction: jump zero 304     | t0: 5 , t1: 0 , t2: 0 , t3: 0 , a0: 8 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 64
pc: 304    | instruction: nop               | t0: 5 , t1: 0 , t2: 0 , t3: 0 , a0: 8 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 64
pc: 312    | instruction: je t0 zero 32     | t0: 5 , t1: 0 , t2: 0 , t3: 0 , a0: 8 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 64
pc: 320    | instruction: addI a0 a0 1      | t0: 5 , t1: 0 , t2: 0 , t3: 0 , a0: 8 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 64
pc: 328    | instruction: swo a0 0          | t0: 5 , t1: 0 , t2: 0 , t3: 0 , a0: 9 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 64
pc: 336    | instruction: subI t0 t0 1      | t0: 5 , t1: 0 , t2: 0 , t3: 0 , a0: 9 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 72
pc: 344    | instruction: jump zero 304     | t0: 4 , t1: 0 , t2: 0 , t3: 0 , a0: 9 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 72
pc: 304    | instruction: nop               | t0: 4 , t1: 0 , t2: 0 , t3: 0 , a0: 9 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 72
pc: 312    | instruction: je t0 zero 32     | t0: 4 , t1: 0 , t2: 0 , t3: 0 , a0: 9 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 72
pc: 320    | instruction: addI a0 a0 1      | t0: 4 , t1: 0 , t2: 0 , t3: 0 , a0: 9 , a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 72
pc: 328    | instruction: swo a0 0          | t0: 4 , t1: 0 , t2: 0 , t3: 0 , a0: 10, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 72
pc: 336    | instruction: subI t0 t0 1      | t0: 4 , t1: 0 , t2: 0 , t3: 0 , a0: 10, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 80
pc: 344    | instruction: jump zero 304     | t0: 3 , t1: 0 , t2: 0 , t3: 0 , a0: 10, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 80
pc: 304    | instruction: nop               | t0: 3 , t1: 0 , t2: 0 , t3: 0 , a0: 10, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 80
pc: 312    | instruction: je t0 zero 32     | t0: 3 , t1: 0 , t2: 0 , t3: 0 , a0: 10, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 80
pc: 320    | instruction: addI a0 a0 1      | t0: 3 , t1: 0 , t2: 0 , t3: 0 , a0: 10, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 80
pc: 328    | instruction: swo a0 0          | t0: 3 , t1: 0 , t2: 0 , t3: 0 , a0: 11, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 80
pc: 336    | instruction: subI t0 t0 1      | t0: 3 , t1: 0 , t2: 0 , t3: 0 , a0: 11, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 88
pc: 344    | instruction: jump zero 304     | t0: 2 , t1: 0 , t2: 0 , t3: 0 , a0: 11, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 88
pc: 304    | instruction: nop               | t0: 2 , t1: 0 , t2: 0 , t3: 0 , a0: 11, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 88
pc: 312    | instruction: je t0 zero 32     | t0: 2 , t1: 0 , t2: 0 , t3: 0 , a0: 11, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 88
pc: 320    | instruction: addI a0 a0 1      | t0: 2 , t1: 0 , t2: 0 , t3: 0 , a0: 11, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 88
pc: 328    | instruction: swo a0 0          | t0: 2 , t1: 0 , t2: 0 , t3: 0 , a0: 12, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 88
pc: 336    | instruction: subI t0 t0 1      | t0: 2 , t1: 0 , t2: 0 , t3: 0 , a0: 12, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 96
pc: 344    | instruction: jump zero 304     | t0: 1 , t1: 0 , t2: 0 , t3: 0 , a0: 12, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 96
pc: 304    | instruction: nop               | t0: 1 , t1: 0 , t2: 0 , t3: 0 , a0: 12, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 96
pc: 312    | instruction: je t0 zero 32     | t0: 1 , t1: 0 , t2: 0 , t3: 0 , a0: 12, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 96
pc: 320    | instruction: addI a0 a0 1      | t0: 1 , t1: 0 , t2: 0 , t3: 0 , a0: 12, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 96
pc: 328    | instruction: swo a0 0          | t0: 1 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 96
pc: 336    | instruction: subI t0 t0 1      | t0: 1 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104
pc: 344    | instruction: jump zero 304     | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104
pc: 304    | instruction: nop               | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104
pc: 312    | instruction: je t0 zero 32     | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104
pc: 352    | instruction: nop               | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104
pc: 360    | instruction: addI dr dr 1      | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 14, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104
pc: 368    | instruction: ret               | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 15, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104
pc: 136    | instruction: addI sp sp 1      | t0: 0 , t1: 0 , t2: 0 , t3: 0 , a0: 13, a1: 0 , a2: 0 , s0: 0 , s1: 0 , s2: 0 , zero: 0, dr: 15, ra: 120, sp: 4093, tr: 0 | rin : 0, rout: 104

Exit code: |-
Halt: Stopping execution
Stdout: |-
Hello, World!
Total: |-
108 instructions executed
```
Пример проверки исходного кода:
```bash
$ cabal run Golden-tests
Slang golden tests
  hello: OK
  fact:  OK
  cat:   OK
  prob5: OK (0.04s)

All 4 tests passed (0.04s)
```
```bash
$ cabal run Unit-tests
Cases: 9  Tried: 1  Errors: 0  Failures: 0
Cases: 9  Tried: 2  Errors: 0  Failures: 0
Cases: 9  Tried: 3  Errors: 0  Failures: 0
Cases: 9  Tried: 4  Errors: 0  Failures: 0
Cases: 9  Tried: 5  Errors: 0  Failures: 0
Cases: 9  Tried: 6  Errors: 0  Failures: 0
Cases: 9  Tried: 7  Errors: 0  Failures: 0
Cases: 9  Tried: 8  Errors: 0  Failures: 0
Cases: 9  Tried: 9  Errors: 0  Failures: 0
```

## Алгоритмы

| ФИО                          | Алг   | LoC | code байт | code инстр. | инстр. | такт. | вариант                                                                               |
|------------------------------|-------|-----|-----------|-------------|--------|-------|---------------------------------------------------------------------------------------|
| Баранов Вячеслав Григорьевич | hello      | 1   | 944       | 118         | 108    | 108   | miranda , risc , harv , hw , instr , struct , stream , port , pstr , prob5 , pipeline |
| Баранов Вячеслав Григорьевич | cat        | 1   | 516       | 129         | 115    | 115   | miranda , risc , harv , hw , instr , struct , stream , port , pstr , prob5 , pipeline |
| Баранов Вячеслав Григорьевич | hello user | 1   | 1288      | 161         | 301   | 301    | miranda , risc , harv , hw , instr , struct , stream , port , pstr , prob5 , pipeline |
| Баранов Вячеслав Григорьевич | prob5      | 4   | 2856      | 357         | 7693   | 7693  | miranda , risc , harv , hw , instr , struct , stream , port , pstr , prob5 , pipeline |
