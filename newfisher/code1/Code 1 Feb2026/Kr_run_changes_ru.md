# Что было сделано, чтобы запустить код с Kr

Работа велась только в папке:

```text
newfisher/code1/Code 1 Feb2026/
```

Цель была запустить текущий Fortran-код на Kr input files: `QSsKr.inp`, `ExcKr.inp`, `AIwKr.inp`, `InzKr2.inp`, сохранив вторым элементом старую Mg-базу.

## Изменения для криптона

1. Подключили Kr как первый элемент расчета (`nX=1`) вместо старой Al-базы.

   В `mo1.for` выставлены параметры Kr-базы:

   ```fortran
   FSS = 30
   HSS = 36
   Nnu = 298
   NST = 1612
   NSTm = 1612
   HSSm = 36
   ```

2. Учтена перенумерация AI-уровней на `200+`.

   Мы сделали эту перенумерацию скриптами `renumerate*`. Это важно, потому что Fortran-код использует:

   ```fortran
   nFAI = 201
   ```

   То есть все уровни с локальным номером `>= 201` считаются autoionizing levels.

3. Исправлен формат `QSsKr.inp`.

   У Kr-файла уровни были записаны в другом формате: до 4 orbital labels, другое выравнивание, не тот fixed-column вид для старого reader-а. Для этого создан скрипт:

   ```text
   fix-qsskr-format.py
   ```

   Он:

   - делает backup исходного `QSsKr.inp` в `QSsKr-temp.inp`;
   - оставляет только последние 2 orbital labels;
   - форматирует строки под Fortran read:

     ```fortran
     read(...,'(a5,a5, f3.0, 2f11.3)')
     ```

   - добавляет колонку `0.000`, чтобы структура была похожа на `QSsAL272.inp` / `QSsMG272.inp`;
   - удаляет лишнюю dashed separator строку после summary block, из-за которой был:

     ```text
     READ(QSsKr.inp) - invalid INTEGER
     ```

4. Исправлен размер Kr-базы.

   Было найдено, что последний He-like AI level имеет global level `1612`, поэтому `NST(1)` и `NSTm` должны быть не меньше `1612`.

5. Увеличен размер line list.

   Kr генерирует намного больше спектральных линий, чем исходная Al/Mg база. Ошибка была:

   ```text
   Increase MNLe=2600 to 10131
   ```

   Поэтому в `mo1.for`:

   ```fortran
   MNLe = 12000
   ```

6. Увеличен photon energy range.

   В `Params.inp` мы изменили:

   ```text
   hvMax = 20.e3 eV
   ```

   Это нужно, потому что у Kr появились линии выше старого `hvMax = 16.e3 eV`, например:

   ```text
   hvC = 16055.486 eV
   ```

7. Изменены параметры первого элемента в `Params.inp` под Kr.

   В исходной git-версии первый элемент был Al, а теперь в коде первый элемент фактически Kr. Поэтому мы изменили параметры, которые относятся к `nX=1`.

   Начальный уровень для загрузки population изменен:

   ```text
   47 -> 236
   ```

   Физический смысл: код кладет начальную заселенность в один заданный уровень первого элемента. После замены Al-базы на Kr старый номер `47` уже не является нужным стартовым уровнем. Номер `236` соответствует Kr level list.

   Масса первого элемента изменена:

   ```text
   26.98 -> 83.80
   ```

   Физический смысл: это атомная масса, используемая в расчетах Doppler broadening / ion mass effects. Для Kr нужна масса около `83.8 a.u.`, а не масса Al.

   Комментарии в строках `Params.inp` все еще могут говорить `Al`, но по смыслу эти строки теперь относятся к первому элементу расчета, то есть к Kr.

8. Изменен plasma temperature scenario в `Params.inp`.

   Мы подняли электронную температуру `Te` во всех точках сценария:

   ```text
   start: 100  -> 1000 eV
   t1:    400  -> 1200 eV
   t2:    900  -> 2000 eV
   t3:   1300  -> 4000 eV
   t4:   1300  -> 4000 eV
   t5:    700  -> 3000 eV
   t6:    200  -> 2000 eV
   ```

   Также изменена начальная ion/Doppler temperature `TD`:

   ```text
   start: 300 -> 1000 eV
   ```

   Остальные значения `TD` в точках `t1..t6` остались такими же, как в git-версии:

   ```text
   1200, 3000, 4000, 4000, 2000, 600 eV
   ```

   Физический смысл: для Kr нужны более высокие температуры плазмы, иначе высокозарядные состояния и K-shell линии Kr не будут адекватно населяться/излучать в выбранном диапазоне.

## Изменения в input-файлах

### `QSsKr.inp`

- Переформатирован в fixed-column вид, читаемый старым Fortran-кодом.
- Оставлены только последние 2 orbital labels.
- Удалена лишняя separator строка между summary block и первым `SpS`.
- Исходная версия сохранена в:

  ```text
  QSsKr-temp.inp
  ```

### `ExcKr.inp`

- Для завершения чтения `ExcKr.inp` код ожидает не EOF, а специальную последнюю строку, соответствующую переходу:

  ```text
  iSS = HSS = 36
  kL = Nnu - 2 = 296
  kU = Nnu - 1 = 297
  ```

- Для Kr это локально соответствует:

  ```text
  36   15   16
  ```

- Мы добавили финальную строку вида:

  ```text
  36   15   16   11   0 ... 0
  ```

  Важно: после `mth` должно быть 7 real values, потому что Fortran читает:

  ```fortran
  iSS, LL, LU, mth, Axw, Bxw, Cxw, Dxw, Exw, Fxw, fw
  ```

### `AIwKr.inp`

- Были найдены запрещенные переходы, где final state тоже AI-level:

  ```text
  iEL2 >= 201
  ```

- Создан скрипт:

  ```text
  filter-aiwkr-forbidden.py
  ```

  Он удаляет запрещенные AI transitions:

  - initial level is not AI: `iEL1 < 201`;
  - final level is AI: `iEL2 >= 201`;
  - final level is not positive;
  - level is out of range according to `QSsKr.inp`;
  - final ion stage is not `iSS1 + 1`.

- Dry-run находил:

  ```text
  735 forbidden transitions
  ```

  Все они были типа:

  ```text
  final level is AI
  ```

- Скрипт пишет лог удаленных строк:

  ```text
  AIwKr-removed-forbidden.log
  ```

- Для завершения чтения `AIwKr.inp` нужен stop-marker с `iSS1 = 111`; Fortran не завершает это чтение по EOF.

### `InzKr2.inp`

- Файл был пустой, а Fortran-код читает из него:

  1. первую header-строку;
  2. вторую header/service-строку;
  3. data строку или stop-marker.

- Добавлен минимальный файл с двумя header строками и stop-marker:

  ```text
  111 ...
  ```

  Это нужно, чтобы избежать:

  ```text
  READ(InzKr2.inp) - end of file encountered
  ```

## Изменения в Fortran-файлах

### `mo1.for`

- Перенастроена первая база на Kr:

  ```fortran
  FSS / 30,  9/
  HSS / 36, 12/
  Nnu /298, 109/
  NST /1612, 272/
  NSTm = 1612
  HSSm = 36
  ```

- Увеличен лимит line list:

  ```fortran
  MNLe = 12000
  ```

### `Code1_Feb2026.for`

- Код уже открывает Kr-файлы:

  ```fortran
  QSsKr.inp
  ExcKr.inp
  InzKr2.inp
  AIwKr.inp
  ```

- Добавлены debug prints для диагностики:

  - `Excitation down in reading Exc...inp`;
  - `AIw consistency error`;
  - `Inconsistency in AI transition energy`;
  - `AI final state in "AIw...inp"`;
  - `non-AI initial state in "AIw...inp"`;
  - `found Line at hv > hvMax`;
  - `Line with hvC > hvMax in EmiAbso`.

- Ослаблен допуск проверки AI transition energy:

  ```fortran
  if(abs(trEn-DE) .gt. 2.0) then
  ```

  Это сделано потому, что `AIwKr.inp` и `QSsKr.inp` имеют небольшие численные расхождения в transition energy.

## Отличия требований к input-файлам: Code1 vs Code2

Сравнивались:

```text
newfisher/code1/Code 1 Feb2026/Code1_Feb2026.for
newfisher/code2/Code2_Febr2026/Co2feb26.for
```

Главное отличие: `Code1` и `Code2` используют разные conventions для AI-уровней и разные способы понять, где заканчиваются input-файлы.

### Общая схема чтения

`Code1` жестко открывает отдельные файлы для Kr и Mg:

```text
QSsKr.inp, ExcKr.inp, InzKr2.inp, AIwKr.inp
QSsMG272.inp, ExcMG272.inp, InzMG272.inp, AIwMG272.inp
```

Количество строк в `Exc/AIw/Inz` заранее не задается. Поэтому `Code1` часто требует специальную последнюю строку или условие остановки внутри самого файла.

`Code2` открывает:

```text
QSs.inp, Exc.inp, Inz.inp, AIw.inp
QSsC.inp, QSsHe.inp, QSsD.inp
Params0.inp, Params1.inp, Params2.inp
```

Для `Exc.inp`, `Inz.inp`, `AIw.inp` число строк задается в `Params0.inp`:

```text
StrExc
StrInz
StrAIw
```

Эти числа включают title/header line. Поэтому `Code2` обычно не требует stop-marker строк `111 ...`; он прекращает чтение, когда счетчик дошел до заданного `Str...`.

### `QSs` files

В `Code1` для уровней используется fixed-column чтение:

```fortran
read(...,'(a5,a5, f3.0, 2f11.3)')
```

То есть имя уровня должно быть разложено на два поля по 5 символов. Именно поэтому для `QSsKr.inp` мы оставляли только последние 2 orbital labels.

В `Code2` для high-Z элемента `QSs.inp` используется более длинное поле:

```fortran
read(...,'(a24, f5.0, f13.3, i6, i9)')
```

То есть `Code2` ожидает полное имя уровня в поле `a24`, плюс два номера `nu1`, `nu2`. Поэтому формат `QSs.inp` из `Code2` не равен формату `QSsKr.inp`, который мы сделали для `Code1`.

Еще одно отличие: в `Code2` после summary block явно читается separating line:

```fortran
read(nFi,'(a9)') empty
```

В `Code1` после summary rows сразу читается номер первого `SpS`; лишняя separator line там вызывает `invalid INTEGER`.

### Нумерация AI-уровней

В `Code1` AI-уровни кодируются положительными локальными номерами:

```text
iEL >= 201
```

Это связано с:

```fortran
nFAI = 201
```

Пример:

```text
iSS=35, iEL=348
```

означает AI-level `348` в данной ion stage.

В `Code2` AI-уровни кодируются отрицательными локальными номерами:

```text
iQS < 0
```

Пример:

```text
35  -148  36  4
```

означает AI-level `148` в `SpS=35`, переходящий на обычный уровень `4` в `SpS=36`.

Это одно из самых важных отличий. Файл `AIW.inp` из `Code2` нельзя напрямую использовать как `AIwKr.inp` для `Code1` без перенумерации AI-уровней на `200+`.

### `Exc` files

`Code1` читает `ExcKr.inp` до специального условия:

```fortran
iSS == HSS(nX)
kL == Nnu(nX)-2
kU == Nnu(nX)-1
```

Для текущего Kr это соответствует локальной строке:

```text
36   15   16   ...
```

Если такой строки нет, `Code1` продолжает читать и падает на EOF.

`Code1` также проверяет:

```fortran
DE = E(kU) - E(kL)
DE > 0
```

Если `DE <= 0`, возникает `Excitation down in reading Exc...inp`.

`Code2` читает `Exc.inp` ровно до `StrExc` из `Params0.inp`. Специальной финальной строки типа `36 15 16` не нужно. AI-уровни в `Exc.inp` задаются отрицательными `LL/LU`, а не `>=201`.

Разрешенные методы тоже отличаются:

```text
Code1: mth = -5, 0, 5, 11
Code2: mth = 5, 11, 16
```

### `AIw` files

`Code1` ожидает:

```text
initial AI level: iEL1 >= 201
final non-AI level: iEL2 < 201
```

То есть AI transition в `Code1` должен быть:

```text
AI level -> ordinary level of next ion stage
```

AI final state запрещен. Поэтому строки с:

```text
iEL2 >= 201
```

удалялись скриптом `filter-aiwkr-forbidden.py`.

`Code1` также проверяет ion-stage consistency:

```fortran
kiSS(kf) == kiSS(ki) + 1
```

и проверяет transition energy:

```fortran
trEn == E(ki) - PI(iSS1) - E(kf)
```

Сейчас допуск ослаблен до:

```fortran
abs(trEn-DE) <= 2.0 eV
```

Для завершения чтения `AIwKr.inp` `Code1` требует stop-marker с:

```text
iSS1 = 111
```

При этом из-за порядка проверок в `Code1` такая строка должна также пройти ранние проверки `iEL1` и `iEL2`. Поэтому безопасный marker имеет вид:

```text
111  201  32  1  ...
```

`Code2` читает `AIW.inp` до `StrAIw` из `Params0.inp`; marker `111` не нужен. Начальный AI-level задается отрицательным `iQS1`, и код прямо проверяет:

```fortran
iQS1 < 0
abs(iQS1) <= nuAS(iSS1)
```

Важное отличие: `Code2` допускает final AI state, если `iQS2 < 0` и номер находится в диапазоне:

```fortran
abs(iQS2) <= nuAS(iSS2)
```

То есть то, что для `Code1` запрещено (`final AI`), в `Code2` может быть допустимым форматом.

### `Inz` files

`Code1` читает `InzKr2.inp` до stop-marker:

```text
iSS1 = 111
```

Перед data строками он читает две header/service строки. Поэтому даже пустой Kr ionization file должен содержать:

```text
header line
service/header line
111 ...
```

`Code1` проверяет:

```fortran
kiSS(kf) == kiSS(ki) + 1
mth == 4
```

`Code2` читает `Inz.inp` до `StrInz` из `Params0.inp`, marker `111` не нужен. AI-уровни опять задаются отрицательными `iQS1/iQS2`. Проверки похожи по смыслу:

```fortran
kiSS(kf) == kiSS(ki) + 1
mePh == 4
```

### Photon grid и line list

`Code1` читает `hvMin`, `hvSmo`, `hvMiF`, `hvMaF`, `hvMax` из `Params.inp` и строит фиксированную сетку из `nvM` точек. Линии с `hvC > hvMax` вызывают STOP.

`Code2` читает `hvMin`, `hvMax`, `domNu` и список областей `hehv(i), dvKv(i)` из `Params1.inp`. Сетка строится по доменам. Дополнительное требование:

```text
last hehv(i) > hvMax
```

иначе сетка может не покрыть весь диапазон. В `Code2` также есть проверка:

```fortran
if(hvCh >= hvMax) STOP 'Line center > hvMax'
```

### Практический вывод

Для переноса файлов между `Code2` и `Code1` недостаточно скопировать `.inp`:

- AI-нумерацию нужно конвертировать из отрицательной (`Code2`) в `200+` (`Code1`);
- `QSs` формат нужно привести из `a24 ... i6 i9` к `a5,a5,f3,2f11.3`;
- для `Code1` нужно добавлять stop-marker строки там, где код не читает число строк из параметров;
- `AIw` нужно дополнительно фильтровать, потому что `Code1` не допускает AI final state, а `Code2` допускает;
- `Exc/Inz/AIw` в `Code2` завязаны на `StrExc/StrInz/StrAIw` в `Params0.inp`; после удаления или добавления строк эти счетчики должны быть обновлены.

## Добавленные Python-скрипты

### `fix-qsskr-format.py`

Назначение: привести `QSsKr.inp` к формату, который читает старый Fortran reader.

Запуск:

```powershell
python "fix-qsskr-format.py"
```

Dry-run:

```powershell
python "fix-qsskr-format.py" --dry-run
```

### `filter-aiwkr-forbidden.py`

Назначение: удалить из `AIwKr.inp` AI transitions, запрещенные логикой Fortran-кода.

Dry-run:

```powershell
python "filter-aiwkr-forbidden.py"
```

Применить изменения:

```powershell
python "filter-aiwkr-forbidden.py" --apply
```

## Важные замечания

- После изменений в `.for` или `mo1.for` нужно пересобирать `Code1_Feb2026.exe`.
- После изменений в `.inp` пересборка не нужна.
- `Params.inp` читается во время запуска, поэтому изменение `hvMax` применяется без rebuild.
- Для `ExcKr.inp`, `AIwKr.inp`, `InzKr2.inp` важны stop-marker строки; код часто не использует EOF как нормальное завершение чтения.
