# Скрипты подготовки и проверки баз

Эта документация описывает основные Python-скрипты репозитория для подготовки
NOMAD-compatible `INP` файлов и параллельного Fisher-набора (`QSs`, `Inz`).
Fortran runtime Code1/Code2 сюда не входит; отличия их input-форматов см.
`newfisher/code1/Code 1 Feb2026/Kr_run_changes_ru.md`.

Старые внешние инструкции существуют в Google Docs:

- `Инструкции`: https://docs.google.com/document/d/198r63lxs8wWXBCXrDYEIunUKANT94P7CSRJyQAS95xs/edit?usp=sharing
- `Nomad - компиляция`: https://docs.google.com/document/d/1SSdkISo7m8nAYs-itV5ttdhNIYoIZDM-5Q-SE1zFMFY/edit?usp=sharing
- дополнительный документ: https://docs.google.com/document/d/1fqjtsW3E4yNd0gXV95XFlgaMLLPUZoaC390QzdL5488/edit?usp=sharing

Все три документа прочитаны через Google Docs text export. Документы могут быть
устаревшими, поэтому актуальное поведение ниже сверено с кодом в этом
репозитории.

## Рабочее окружение

Старые инструкции описывают Windows-окружение:

- открыть командную строку Windows;
- перейти в директорию проекта (`d:`, затем `cd plasma` или актуальный путь);
- обновить код через `git pull`;
- запускать нужный Python-скрипт из корня репозитория.

Для исходной установки упомянуты:

- Git for Windows;
- Python 3.11 или новее;
- Python packages `numpy`, `scipy`;
- Strawberry Perl;
- Perl модуль `IO::CaptureOutput` через `cpan IO::CaptureOutput`;
- Gzip в `PATH` на Windows;
- UnixUtils в `PATH`;
- доступность команд `git`, `python`, `perl`, `gzip` из новой консоли.

Первичная установка кода:

```powershell
git clone https://github.com/pavelber/plasma.git
```

Обновление кода:

```powershell
git pull
```

Проверка окружения в старых инструкциях:

```powershell
python env.py
```

В текущем репозитории часть скриптов также требует внешние Perl/Fortran tools,
пути к которым берутся из `lib/env.py`/конфигурации окружения.

## Основные файлы базы

Скрипты работают с комплектом файлов:

- `IN1.INP`: уровни по спектроскопическим номерам.
- `EXCIT.INP`: возбуждение внутри одного спектроскопического номера.
- `SPECTR.INP`: радиационные линии внутри одного спектроскопического номера.
- `RREC.INP`: рекомбинация из `Z` в `Z+1`.
- `BCFP.INP`: ионизация из `Z` в `Z+1`.

Для согласованной базы все переходы в этих файлах должны ссылаться только на
species/levels, существующие в `IN1.INP`.

## FAC -> INP

Entry point:

```powershell
python run.py "C:\path\to\fac-input" "C:\path\to\out" 1e4
```

В старых инструкциях эта команда описана как:

```powershell
python run.py directory-with-fac output-dir min-einstein-coefficient
```

Входная директория должна содержать поддиректории по спектроскопическим номерам
с FAC файлами (`fac.lev`, `fac.tr`, `fac.rr`, `fac.ce`, `fac.ci`, `fac.ai`).

Скрипт:

- конвертирует FAC файлы во внутренний старый формат;
- запускает промежуточные Perl/Fortran tools;
- собирает общие `AIW.INP`, `BCFP.INP`, `EXCIT.INP`, `RREC.INP`, `IN1.INP`,
  `SPECTR.INP`;
- применяет проверки и исправления из `lib/check_and_fix.py`;
- создает `WARNINGS.txt` с предупреждениями и полезной диагностикой.

UI entry point из старых инструкций:

```powershell
python run-ui.py
```

Особенность: при верхней границе базы могут появляться top-edge переходы на
следующий species, которого нет в `IN1.INP`. Такие переходы надо либо
согласованно поддерживать добавлением следующего species, либо удалять при
сборке/merge, если база должна заканчиваться на текущем species.

## Legacy check/fix utilities

Каталог `check/` содержит Perl wrappers и fixers, которые копируются в рабочие
директории перед проверками. Сами проверяющие бинарники собираются из Fortran
исходников в `ralchenko/src`:

- `check.f` -> `check.exe`: проверяет `EXCIT.INP`, пишет/печатает bad lines для
  `fix_excit.pl`.
- `check_bcfp.f` -> `check_bcfp.exe`: проверяет `BCFP.INP`, используется
  `fix_bcfp.pl`.
- `check_rr.f` -> `check_rr.exe`: проверяет `RREC.INP`, пишет `check_rout`,
  который затем читает `fix_rr.pl`.

Полное описание всех программ Ральченко (фиты `exc_fac`/`ph_fac`, формулы,
старая версия `exc_fac.exe`) — в [ralchenko.md](ralchenko.md).

Сборка старого набора описана в `ralchenko/src/Makefile`:

```powershell
make all
```

В `check/` лежат:

- `check_all.pl`: общий Perl orchestrator. С опцией `-d` проходит по всем
  поддиректориям текущей директории; без `-d` проверяет текущую директорию.
- `fix_excit.pl`: читает `check_out`, правит или удаляет плохие строки
  `EXCIT.INP`, затем сортирует результат.
- `fix_bcfp.pl`: запускает/читает `check_bcfp`, удаляет плохие ionization
  transitions из `BCFP.INP`.
- `fix_rr.pl`: читает `check_rout`, правит или удаляет плохие строки
  `RREC.INP`, затем сортирует результат.
- `old_fix_rr.pl`: старая версия правки `RREC.INP`, используется как fallback.
- `flx.pl`: вспомогательный Perl script из старого набора check/fix utilities.

Python-обвязка находится в `lib/check_and_fix.py`:

- `copy_checks(my_dir, out_dir)`: копирует файлы из `check/` в output directory
  и во все числовые поддиректории.
- `check_and_fix_rr(dir)`: в цикле запускает `check_rr.exe` и `perl fix_rr.pl`
  до исчезновения bad lines или до лимита итераций.
- `check_and_fix_rr_version2(rec_dir)`: новая Python-логика для `RREC.INP`;
  читает bad lines из `check_rr.exe` и правит строки в Python через
  `run_new_fix()`.
- `check_and_fix_old_rr(dir)` и `check_and_fix_old_rr_version2(rec_dir)`:
  fallback через `old_check_rr.exe`/`old_fix_rr.pl`.
- `check_fix(...)`: helper для database-based pipeline; копирует checks в
  species-директорию и прогоняет `check_and_fix_rr_version2`.

Где вызывается:

- `run.py`:
  - после запуска FAC tools вызывает `copy_checks()` и `perl check_all.pl -d`
    для числовых поддиректорий;
  - затем для каждой числовой директории вызывает `check_and_fix_rr(...)`;
  - после сборки общих файлов вызывает `check_and_fix_rr(out_dir)`;
  - в конце `check_and_fix_in_main_dir()` запускает `perl check_all.pl`,
    `check_and_fix_rr(out_dir)` и `check_and_fix_old_rr(out_dir)`.
- `create_all_from_databases.py`:
  - вызывает `check_fix(...)` для per-species `RREC.INP`;
  - затем вызывает `check_and_fix_rr_version2(elem_dir)` и
    `check_and_fix_old_rr_version2(elem_dir)` для общей директории.
- `run-ui.py` и `ui-databases.py` вызывают те же Python helpers из
  `lib/check_and_fix.py` из UI workflows.

Эти legacy checks отличаются от нового `scripts/validate_database.py`: они не
проверяют общую ссылочную целостность базы, а в основном пытаются исправить
численные/форматные проблемы в `EXCIT`, `BCFP`, `RREC`. Новый валидатор лучше
запускать после legacy fixers как финальную проверку согласованности файлов.

## Внешние базы -> INP

Entry point:

```powershell
python create_all_from_databases.py --out-dir "C:\path\to\out" --element Ti --nmax 10 --energy-limits "..." --min-sp-num 1 --max-sp-num 6
```

В Scripts HowTo встречается устаревшая позиционная форма:

```powershell
python create_all_from_databases.py C:\work4\db C 6 1e-8 "1:70.8,2:150,3:250,4:350,5:450,6:550,7:750" 1 8
```

Текущий код использует named arguments или `--config`; при расхождении надо
ориентироваться на `parse_args_and_config()` в `create_all_from_databases.py`.

Также поддерживается режим с INI-конфигом:

```powershell
python create_all_from_databases.py --config "C:\path\to\config.ini"
```

Скрипт строит NOMAD-базу из уже лежащих в `db/<element>/` данных Питера
(University of Kentucky) и сразу пытается сделать рядом Fisher-файлы.

Порядок шагов в `create_all_from_databases.py`:

1. Копирует `db/<elem>/levels` и `db/<elem>/lines` в `<out>/<elem>/`.
2. Строит `IN1.INP`, `EXCIT.INP`, `SPECTR.INP` из Питера
   (`lib/create_in1_from_databases.py`).
3. Строит `RREC` и `BCFP.INP` из `IN1` (переходы по конфигурациям).
4. Склеивает per-species `RREC.INP`, гоняет legacy check/fix.
5. Удаляет неиспользуемые уровни и перенумеровывает ссылки.
6. Пытается подставить сечения ионизации/возбуждения из
   `db/<elem>/ionization-crosssection` и `db/<elem>/excitation-crosssection`.
   Если файла сечения нет, ионизация фитится с табуляции формулы
   Бернштама–Радченко или Лотца (`lib/bcfp.py` + `lib/cross_section.py`).
7. Если задан `--replace-starting-from`, подмешивает FAC из `db/<elem>/fac`.
8. Пишет Fisher-файлы через `run_for_fisher()`.

`--formula` относится только к `RREC` (формула vs Strasbourg cuts), не к
ионизации. Ионизация Бернштамом/Лотцем идёт всегда как fallback, без флага.

Флаг `--formula` сейчас фактически не работает из CLI/INI: после разбора
аргументов стоит `use_formula = (use_formula == 'formula')`, а к этому
моменту значение уже `bool`, поэтому в `create_rrec_from_in1` всегда уходит
`False` (Strasbourg). UI `ui-databases.py` этот флаг передаёт иначе и
Fisher-шаг не вызывает.

`nmax` передаётся в `should_include_level()`, но там не используется: уровни
с главным квантовым числом `n > 6` отбрасываются жёстко.

`--energy-limits` — это отсечка энергии уровня при сборке `IN1`, не
автоматический ionization potential. IP в заголовок `IN1` берётся отдельно
из строки `Limit` в Piter levels-файле.

Опция `--replace-starting-from` подключает FAC-часть из `db/<element>/fac`,
начиная с указанного спектроскопического номера. Это общий механизм, но для
ручного Ti stitch по таблице соответствий лучше использовать отдельный скрипт
`merge-titanium-piter-fac.py`.

## Fisher из внешних баз (Питер)

Fisher-файлы создаются тем же `create_all_from_databases.py`, не отдельным
скриптом. Выход — каталог `<out>/<elem>/fisher/`:

- `QSs<elem><N>.inp` — уровни из `IN1.INP` (`lib/create_qsege.py`, режим
  `use_fac_lev=False`, то есть без `fac.lev`);
- `Inz<elem><N>.INP` — ионизация электронным ударом + фотоионизация из
  `RREC`.

Нужны `QSs`, `Exc`, `Inz`, `AIw`. `Params` не нужен.

Текущий пайплайн пишет только `QSs` и `Inz`. Отдельного Fisher-конвертера
для `Exc`/`AIw` нет — и для `Exc` он, скорее всего, не нужен.

`EXCIT.INP` (NOMAD) и `Exc*.inp` (Fisher) — один формат: тот же заголовок
`SS #1 #2 Mthd A B C D E F Osc.Strngth` и те же колонки. Питер-ветка уже
пишет `EXCIT.INP` в `create_spectr_and_excit_from_piter_match_config()`.
Для Fisher достаточно положить этот файл как `Exc.inp` / `Exc<elem>.inp`.
`create_excit()` в `lib/create_files_union.py` — это FAC-склейка `outpp.dat`,
не другой формат.

`AIW.INP` (NOMAD) и `AIw*.inp` (Fisher) тоже с одним заголовком
`SSi AIQS# SSf QSf# WAI DE(eV)`. Код сборки `create_aiw()` есть только для
FAC: читает `fac.ai`. Из Питера автоионизации нет,
`create_in1_from_piter.py` ставит число AI-уровней в `0`. Для этой базы
`AIw` будет пустым (один заголовок), как `db/O/AIW.INP`.
Нумерация AI разная у Code1 (`>= 201`) и Code2 (отрицательные номера),
но без AI-переходов это не важно.

`Inz` читает `BCFP.INP.fisher`. Фиты из `create_fac_fitting_params()` и
обновлённый `EXCIT.INP` дампятся всегда, и после FAC-merge тоже.
Раньше `BCFP.INP.fisher` писался только при `--replace-starting-from`.

Если `BCFP` ещё в format 1 (в заголовке есть `Coefficient`), `createIonFile()`
обнуляет fit-коэффициенты EII. Бернштам-константы (`c_l`, `delta_l`, число
электронов) считаются, но в файл не записываются. Рабочий путь — сначала
получить format 2 (`D, -A, B, C`) через `create_fac_fitting_params()`, потом
собрать `Inz`.

### Сечения ионизации Питера и формулы

Ожидаемый каталог: `db/<elem>/ionization-crosssection/`.
Имена файлов вида `OI-OII_27-12.dat` (элемент + римские sp + уровни).
Парсер: `parse_ionization_spectroscopic_files()` в
`lib/create_cross_section_fits.py`.

В репозитории этих каталогов нет. Скрипта скачивания **электрон-ударных**
сечений ионизации Питера тоже нет: есть только загрузка levels/lines и
Strasbourg photoionization cuts (см. раздел «Загрузка внешних данных»).
`lib/cross_section_db.py` смотрит на другие имена
(`ionization-crosssections` во множественном числе) и из
`create_all_from_databases.py` не вызывается.

Логика одного перехода в `BCFP.create_fac_fitting_params()`:

1. Если есть dat-файл Питера на эту пару уровней — фитить NOMAD-коэффициенты
   по таблице, в комментарий писать `#from db`.
2. Иначе построить аналитическую σ(E):
   - Бернштам–Радченко, если потерянный электрон в оболочке `s`/`p`/`d`;
   - Лотц (`get_lotz_om2`), если число электронов известно, а `l` не из
     `s`/`p`/`d`;
   - пропуск, если конфигурации не сопоставить.
3. «Табулирование» — это выборка формулы на сетке `E = 1..100` шаг `1 eV`
   и fit теми же NOMAD-коэффициентами (`create_fits_from_range`).
   Отдельные txt-таблицы не пишутся: `create_tabulation_files=False`.

`remove_bad_fits=True` (в скрипте есть TODO «поговорить»): неудачный fit
удаляет переход из BCFP, а не оставляет нули.

### Что уже лежит в `db/` для C, O, Ar, Fe, Cu

| Элемент | levels | lines | ionization-crosssection |
|---|---|---|---|
| C | 1–6 | 1–6 | нет |
| O | 1–8 | 1–8 | нет |
| Ar | 1–8 | 1–8 | нет |
| Fe | 1–8 | 1–8 | нет |
| Cu | 1–7 | 1–6 (4–6 почти пустые у Питера) | нет |

Ядро: C=7, O=9, Ar=19, Fe=27, Cu=30. Если `max-sp-num + 1 == nucleus`,
скрипт добавляет nucleus в `RREC`/`BCFP`. Для Ar/Fe/Cu при max=6 этого нет.

## Подготовка FAC уровней по NIST/Piter

Скрипты:

```powershell
python scripts\replace_in_fac_nist.py "C:\path\to\fac" "C:\path\to\out" 4
python scripts\replace_in_fac_piter.py "C:\path\to\fac" "C:\path\to\out" 10
```

В старых инструкциях эта операция называлась корректировкой энергий в `fac.lev`.
В старом документе она запускалась как:

```powershell
python replace_in_fac.py C:\work4\Fe_lev C:\work4\Fe_out 8
```

Последний параметр означает, до каких конфигураций исправлять уровни. В текущем
коде старый `replace_in_fac.py` разделен на NIST/Piter варианты.

Они читают FAC `fac.lev`, скачивают или используют уровни из NIST/Piter,
пересобирают `fac.lev`, перенумеровывают уровни и пишут `fn.corr`.

Назначение из Scripts HowTo:

- `replace_in_fac_piter.py`: создает correction file из University of Kentucky
  database, перенумеровывает и проверяет, что энергии уровней возрастают;
- `replace_in_fac_nist.py`: создает correction file из NIST и проверяет
  возрастание энергий.

Эти скрипты работают на уровне FAC-файлов до общей сборки `INP`.

## Titanium Piter/FAC merge

Entry point:

```powershell
python merge-titanium-piter-fac.py "C:\work4\tmp\Ti" "C:\work4\tmp\Ti2" "C:\work4\tmp\Ti-merged" "C:\work2\plasma\TiMapping.csv"
```

Зависимость: для конвертации `BCFP` branching-ratio строк в fit-coefficients
формат нужен `scipy`.

```powershell
python -m pip install scipy
```

Назначение: собрать длинную Ti-базу из внешней/Piter-части и FAC-части,
сшивая их по Ti6 через ручную CSV-таблицу соответствий уровней.

По умолчанию:

- внешняя/Piter база берется из первого аргумента;
- FAC база берется из второго аргумента;
- результат пишется в третий аргумент;
- mapping читается из CSV;
- boundary species равен `6`;
- FAC-уровень берется из колонки `NOMAD from Fac №`;
- целевой/Piter/NIST уровень берется из колонки `NIST №`.
- энергия для Ti6 берется из колонки `Peter (NIST) energy`.

Что делает скрипт:

- читает `IN1/EXCIT/BCFP/SPECTR/RREC` из обеих баз;
- для Ti6 оставляет только FAC уровни, перечисленные в CSV;
- перенумеровывает Ti6 FAC уровни в целевые номера из CSV;
- удаляет переходы с удаленными Ti6 уровнями;
- для species ниже Ti6 берет внешнюю/Piter часть;
- для species выше Ti6 берет FAC часть;
- удаляет top-edge `RREC`/`BCFP` переходы на species, отсутствующий в итоговом
  `IN1.INP`;
- приводит `BCFP` к единому fit-coefficients формату.

Если CSV имеет другие заголовки колонок, их можно передать явно:

```powershell
python merge-titanium-piter-fac.py "C:\work4\tmp\Ti" "C:\work4\tmp\Ti2" "C:\work4\tmp\Ti-merged" "C:\work2\plasma\TiMapping.csv" --fac-level-column "FacLevel" --target-level-column "PiterLevel" --target-energy-column "PiterEnergy"
```

## Валидация базы

Entry point:

```powershell
python scripts\validate_database.py "C:\work4\tmp\Ti-merged"
```

Валидатор можно запускать на любой базе: FAC-generated, external/Piter/NIST или
merged.

Он проверяет:

- наличие `IN1.INP`, `EXCIT.INP`, `SPECTR.INP`, `RREC.INP`, `BCFP.INP`;
- соответствие counts в header `IN1.INP` фактическим уровням;
- дубликаты уровней и переходов;
- что все переходы ссылаются на существующие species/levels из `IN1.INP`;
- числовые поля в переходах;
- числовую сортировку species и переходов;
- допустимый synthetic top-edge header-only species, если он задан как один
  nucleus level без секции.

Успешная проверка завершается exit code `0` и выводит:

```text
ERRORS: 0
WARNINGS: 0
```

При ошибках exit code равен `1`, а отчет показывает первые примеры. Количество
примеров можно ограничить:

```powershell
python scripts\validate_database.py "C:\work4\tmp\Ti-merged" --max-examples 20
```

## Старый FAC/NIST merge

Entry point:

```powershell
python merge-fac-nist.py "C:\path\to\fac-db" "C:\path\to\nist-db" --replace-starting-from 5
```

Скрипт является старым/экспериментальным механизмом merge FAC и NIST. Он
использует `IN1.add_or_replace_sp_data()` и пытается сопоставлять уровни через
энергию, статистический вес и конфигурацию.

Для Ti merge с ручной таблицей соответствий этот скрипт не подходит напрямую:
используйте `merge-titanium-piter-fac.py`.

## Загрузка внешних данных

Питер (University of Kentucky, `linelist.pa.uky.edu`) качается так:

```powershell
python scripts\download_one_elem.py Fe
```

Это `scripts/download_one_elem.py`: пишет в `db/<element>/levels` и
`db/<element>/lines`. Сейчас вызов `download_piter_levels()` закомментирован,
качаются только lines через `download_piter_lines()`.

Один спектроскопический номер:

```powershell
python scripts\download_one_elem_onesp_num.py
```

`scripts/download_one_elem_onesp_num.py` — элемент и римский номер сейчас
зашиты в файле (`elem = "Na"`, `sp_num = "I"`), их надо править перед запуском.

Массовая загрузка H…Kr:

```powershell
python scripts\download_databases.py
```

`scripts/download_databases.py` качает levels и lines для атомных номеров 1–36.
`nmax = 10`, `osc = 1e-8`, `max_sp_num` ограничен 8.

Все три скрипта lines/levels крутятся по `lib/current_data.py`:
`SPNUMS_TO_USE`. Сейчас там только `['I']`, поэтому
`download_piter_lines()` / `download_piter_levels()` скачают лишь первый
спектроскопический номер, пока список не расширить. Для дырки в одном
species надёжнее `download_piter_lines_one_spnum()`.

### Что есть у Питера и чего нет

Сайт: [Atomic Line List](https://linelist.pa.uky.edu/newpage/)
(levels: [levels.html](https://linelist.pa.uky.edu/newpage/levels.html)).
Это каталог уровней и радиационных линий, не collision-база.

Есть:

- уровни (`db/<elem>/levels`, `qlevels.cgi`);
- радиационные линии и вероятности переходов (`db/<elem>/lines`, `qlines.cgi`);
- ionization limits в levels-файлах (строка `Limit`).

Нет:

- сечений ионизации электронным ударом;
- сечений возбуждения электронным ударом;
- скоростей автоионизации;
- сечений фотоионизации (в этом репо их берёт Strasbourg, не Питер).

Отдельного скрипта под `db/<elem>/ionization-crosssection/`
(файлы вида `OI-OII_27-12.dat`) поэтому нет. Код умеет такие dat подставить,
если положить вручную, но у Питера их взять неоткуда.

Strasbourg (фотоионизация / cuts, не Питер EII):

```powershell
python scripts\download_strasburg.py
```

Пишет `db/<elem>/strasbg-levels` и `db/<elem>/strasbg-cuts`. Элемент и
диапазон сейчас зашиты в скрипте (`elem = "Si"`, cuts 8–9). Эти cuts идут
в `RREC`, если `use_formula` выключен.

## Практический порядок работы

Для новой или измененной базы:

1. Собрать входную базу нужным скриптом.
2. Запустить `scripts\validate_database.py`.
3. Исправить ошибки ссылок, сортировки или top-edge переходов.
4. Повторить validation до `ERRORS: 0`.
5. Только после этого использовать базу дальше.

## Вопросы, которые из кода не закрыть

Имеется в виду задание: базы Fisher для C, O, Ar, Fe, Cu из Питера;
для Ar/Fe/Cu max sp = 6; сечения ионизации электронным ударом брать у
Питера, если есть; дыры добивать табуляцией Бернштама и Лотца.

Ответы (актуальное состояние — в `docs/plan-c-o-ar-fe-cu.md`):

1. C до ядра (1–6 + nucleus). O до 6–7; основа — существующий
   `db/O/IN1.INP` (там 1–8, IP в заголовке).
2. Energy-limits = IP, как у NOMAD-кислорода.
3. До sp 6 — только одиножды возбуждённые. В коде такого фильтра нет,
   есть только отсечка `n > 6`.
4. У Питера EII нет. Ионизация из баз, если dat есть; иначе формулы.
5. `QSs`, `Exc`, `Inz`, `AIw`. `Params` не нужен. `Exc` = `EXCIT.INP`.
6. Дамп `BCFP.INP.fisher` починен.
7. Ионизация: базы если есть, иначе формулы. RREC — Strasbourg.
8. Lines докачаны. Cu IV–VI у Питера почти пустые.
9. Неудачные фиты отмечать (`remove_bad_fits=False`), не удалять.
10. Сетка табуляции 1–100 — да.
11. Формат Fisher Code 2.
