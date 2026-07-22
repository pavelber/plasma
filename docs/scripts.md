# Скрипты подготовки и проверки баз

Эта документация описывает основные Python-скрипты репозитория для подготовки
NOMAD-compatible `INP` файлов. Раздел про Fisher/NOMAD runtime сюда намеренно
не включен.

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

Скрипт строит базу из загруженных/подготовленных внешних данных:

- создает `IN1.INP`, `EXCIT.INP`, `SPECTR.INP`;
- строит `RREC.INP` и `BCFP.INP` из `IN1`;
- добавляет cross-section данные, если они есть;
- удаляет неиспользуемые уровни и перенумеровывает ссылки.

Назначение из Scripts HowTo: создавать NOMAD input files из University of
Kentucky и NIST данных для low-charge ions, где FAC точность хуже.

Опция `--replace-starting-from` подключает FAC-часть из `db/<element>/fac`,
начиная с указанного спектроскопического номера. Это общий механизм, но для
ручного Ti stitch по таблице соответствий лучше использовать отдельный скрипт
`merge-titanium-piter-fac.py`.

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

## Загрузка Piter данных

Entry point:

```powershell
python scripts\download_one_elem.py Ti
```

Скрипт создает/использует `db/<element>/levels` и `db/<element>/lines`.
В текущем коде загрузка levels закомментирована, а lines загружаются через
`download_piter_lines()`.

Перед использованием проверьте параметры внутри скрипта (`nmax`, `osc`,
диапазон species), потому что они сейчас заданы константами в файле.

## Практический порядок работы

Для новой или измененной базы:

1. Собрать входную базу нужным скриптом.
2. Запустить `scripts\validate_database.py`.
3. Исправить ошибки ссылок, сортировки или top-edge переходов.
4. Повторить validation до `ERRORS: 0`.
5. Только после этого использовать базу дальше.
