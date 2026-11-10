# Коды Фишера (Code1, Code2): версии и где что лежит

Коды на Фортране для кинетики атомных уровней (столкновительно-радиационная модель)
и расчёта рентгеновских спектров плазмы. Исходно они собирались компилятором
MS Fortran PowerStation (`fl32`, через nmake) с библиотекой NAG (`naglib.lib`).
GNU-версии собираются gfortran без NAG: две процедуры NAG заменены своими.

- **D01AHF** — адаптивная квадратура Гаусса–Кронрода (15 точек).
- **D02EAF** — матричная экспонента матрицы скоростей `PM`. `PM` на шаге по времени
  заморожена, как и в `POPdot`. Это не общий решатель ОДУ.

## Какую версию брать

| Задача | Папка | Исполняемый файл |
|---|---|---|
| Code2, любой элемент (Kr, Ge, …) + C, He, D | `code2/Code2_Febr2026_Generic` | `Co2feb26_Generic.exe` |
| Code1, Al + Mg | `code1/Code1_AlMg_GNU` | `Code1_AlMg.exe` |
| Code1, Kr + Mg (только MS Fortran + NAG) | `code1/Code 1 Feb2026` | `Code1_Feb2026.exe` |

## Code2 (Co2feb26 и предшественники)

Сферическая мишень: три зоны (BS, Core, Capsule). Четыре элемента: основной элемент X
(Kr, Ge, …) и C, He, D. Входные файлы: `Params0.inp`, `Params1.inp`, `Params2.inp`,
`QSs.inp`, `QSsC.inp`, `QSsHe.inp`, `QSsD.inp`, `Exc.inp`, `Inz.inp`, `AIw.inp`,
`InpResp1-9.inp`. Описание входных параметров есть в руководстве
`code2/orig/Manual_2, January 2026 .pdf`.

| Папка | Что это | Сборка | Документация |
|---|---|---|---|
| `code2/Code2_Febr2026_Generic` | **Основная версия.** Те же исходники для всех элементов: `nQSm = max(NST)` и `HSSm = max(HSS)` берутся из `Params0.inp` при запуске, `MNLe` = число линий + 1. Массивы allocatable. Формат входных файлов тот же. | `mingw32-make -f Makefile.gnu` | `README_Generic.txt` |
| `code2/Code2_Febr2026` | Co2feb26, версия февраля 2026. Оригинал (`Co2feb26.for`, `mo1co2feb.for`, `Makefile` для nmake + NAG) и GNU-перенос (`Co2feb26_GNU.for`, `nag_replacements_Co2_GNU.f90`, `lzfix_Co2_GNU.f90`, `Makefile.gnu`). Размеры заданы при компиляции (`nQSm=1620`, `HSSm=36`, `MNLe=15000`). Входные файлы для Kr. | `mingw32-make -f Makefile.gnu` (GNU) или `nmake` (MS) | только комментарии в `Makefile.gnu` |
| `code2/code2Feb2026-case16-Ge` | Случай Ge (метод возбуждения 16, NST=1107): входные файлы и копия GNU-исходников Co2feb26. `Params2.inp` переведён из старого формата (u3D) в TD; оригинал — `Params2_u3D_orig.txt`. | `mingw32-make -f Makefile.gnu` | `README.txt` |
| `code2/orig` | Co2nov25 (ноябрь 2025), оригинал для MS Fortran + NAG, и руководство `Manual_2, January 2026 .pdf`. | `nmake` | Manual_2 (pdf) |
| `code2/LonGe_Jan_26` | Ge, январь 2026: Co2nov25 и входные файлы Ge в старом формате `Params2`; `Code2.bat` для запуска, `log.txt`. | MS Fortran | нет |
| `code2/old-code2-case16` | Старый `Code2.for` с методом возбуждения 16. | — | нет |
| `../code2` (корень репозитория) | Самая старая версия Code2 (`Code2.for`, `mo1code2.for`) и `Manual_2.pdf`. | `nmake` | Manual_2 (pdf) |

### Проверка GNU-версий Code2

- **Kr: GNU против NAG-эталона.** Полный расчёт до 65 пс. Половина выходных файлов
  совпадает байт в байт. В остальных около 0.1% чисел отличаются на 1 в последнем
  напечатанном знаке; такие же отличия есть в `LineList.dat`, который NAG не использует.
  Это округление при печати, а не физика.
- **Generic против сборки с фиксированными размерами.** Полные расчёты Kr и Ge до 65 пс:
  все 30 выходных .dat файлов совпали байт в байт. Прогоны с проверкой границ массивов
  (`-fcheck=bounds`) прошли без ошибок.

## Code1

Два элемента, параметры расчёта задаются в `Params.inp`. Размеры массивов (FSS, HSS, NST, NSTm, HSSm, MNLe) задаются в модуле `mo1*.for` **при
компиляции**. Поэтому для другого элемента Code1 пока нужно править модуль и
пересобирать; общей (Generic) версии Code1 ещё нет.

| Папка | Что это | Сборка | Документация |
|---|---|---|---|
| `code1/Code1_AlMg_GNU` | Code1 для Al + Mg (по 272 уровня, от Be-подобного иона до ядра), перенос на gfortran без NAG. Две независимые замены NAG: `nag_replacements_AlMg_GNU_v01.f90` (используется) и `nag_replacements_win.f90`; при печати они совпадают. `lzfix_AlMg_GNU_v01.f90` печатает `.ddd` вместо `0.ddd`, как старый компилятор. Полный расчёт идёт около часа. | `mingw32-make` | `Code1_AlMg_GNU_v01_technical_migration_report.docx` |
| `code1/Code 1 Feb2026` | Code1 версии февраля 2026, переделанная под Kr + Mg (Kr — первый элемент, NST=1612, AI-уровни перенумерованы с 201, `MNLe=12000`, `hvMax=20 кэВ`, `hvMaF=17.3 кэВ`). Только MS Fortran + NAG. Скрипты подготовки входных файлов Kr: `renumerate-*.py`, `fix-qsskr-format.py`, `filter-aiwkr-forbidden.py`. | `nmake` | `Kr_run_changes_ru.md` |
| `../code1` (корень репозитория) | Старая версия Code1 (`Code1.for`, модули `mo1AlMg.for`, `mo1KrHe.for`, `mo1KrMg.for`) и опыты с разными библиотеками вместо NAG (`Makefile.netlib`, `.quadpack`, `.slatec`, `.odepack`, …). | разные Makefile | нет |

### Проверка GNU-версии Code1

Al + Mg: полный расчёт сравнён с NAG-эталоном. Все выходные файлы совпадают по
формату. Меньше 0.5% чисел отличаются на 1 в последнем (4-м значащем) знаке.

## Прочее

- `data/Ge_0_2.5_0_all_params` — набор входных файлов Ge для Code2.
- `scripts/create_params0.py` — строит `Params0.inp` (FSS, HSS, Nnu, NST) по файлам `QSs*.inp`.

## Сборка GNU-версий под Windows

1. Установить GNU Fortran, например WinLibs (вместе с ним ставится `mingw32-make`):
   `winget install BrechtSanders.WinLibs.POSIX.UCRT`, затем открыть новый терминал.
2. В папке версии запустить `mingw32-make -f Makefile.gnu` (для Code1 — просто `mingw32-make`).
   Исполняемый файл собирается статически и запускается на любом Windows без MinGW.
   Цели: `debug` (проверка границ массивов, backtrace), `clean`.
3. Запускать в папке с входными файлами: программа пишет выходные файлы (`*.dat`)
   в текущую папку. Выходные файлы, `*.o`, `*.mod` и `*.exe` в git не коммитятся.

Исходники компилируются как стандартный fixed form с 72 колонками, как в MS Fortran:
текст после 72-й колонки игнорируется (в нескольких строках там стоят комментарии без `!`).
