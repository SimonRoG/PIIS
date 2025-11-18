# Quick Start Guide - HIV Diagnosis System (Lisp)

## Швидкий запуск

### Вимоги

- Common Lisp (SBCL, CLISP, або інша реалізація)
- Для установки SBCL на Ubuntu/Debian:
  ```bash
  sudo apt-get install sbcl
  ```

### Запуск демонстрації

```bash
# Перейти до директорії
cd 2/

# Запустити SBCL з файлом
sbcl --load hiv.lisp

# В REPL виконати:
(run-demo)
```

### Інтерактивна діагностика

```lisp
;; В SBCL REPL:
(load "hiv.lisp")
(interactive-diagnosis)
```

### Використання в коді

```lisp
;; Завантажити систему
(load "hiv.lisp")

;; Створити пацієнта
(add-patient "пацієнт-тест"
             '(angina zapalennya-limfovuzliv nudota blyuvannya)
             '(pidvyshchenyy-riven-leykocytiv))

;; Отримати діагноз
(print-diagnosis (get-patient "пацієнт-тест"))

;; Або просто отримати рівень ймовірності
(diagnose-hiv-probability (get-patient "пацієнт-тест"))
;; => висока
```

## Доступні функції

### Основні функції

- `(run-demo)` - Запуск демонстрації з 3 тестовими пацієнтами
- `(interactive-diagnosis)` - Інтерактивний режим діагностики
- `(print-diagnosis patient)` - Вивести повний діагноз пацієнта
- `(diagnose-hiv-probability patient)` - Отримати рівень ймовірності

### Робота з пацієнтами

- `(add-patient name symptoms analyses)` - Додати пацієнта
- `(get-patient name)` - Отримати пацієнта
- `(count-symptoms patient)` - Підрахувати симптоми
- `(has-symptom-p patient symptom)` - Перевірити наявність симптому
- `(has-analysis-p patient analysis)` - Перевірити наявність аналізу

### Перевірка захворювань

- `(has-mononucleosis-symptoms-p patient)` - Чи є симптоми мононуклеозу
- `(has-mononucleosis-analyses-p patient)` - Чи є аналізи мононуклеозу
- `(has-meningitis-symptoms-p patient)` - Чи є симптоми менінгіту
- `(has-encephalitis-symptoms-p patient)` - Чи є симптоми енцефаліту
- `(has-esophagitis-symptoms-p patient)` - Чи є симптоми езофагіту

### Допоміжні функції

- `(what-disease-does-symptom-suggest symptom)` - На що вказує симптом
- `(print-patient-info patient)` - Вивести інформацію про пацієнта

## Приклади симптомів

Всі симптоми:
```lisp
pidvyshchena-temperatura      ; підвищена температура
angina                         ; ангіна
zapalennya-limfovuzliv        ; запалення лімфовузлів
slabkist                       ; слабкість
rozbytist                      ; розбитість
potovidylennya                 ; потовиділення
golovnyy-bil                   ; головний біль
znyzhennya-apetytu             ; зниження апетиту
porushennya-snu                ; порушення сну
zbilshennya-pechinky           ; збільшення печінки
zbilshennya-selezinky          ; збільшення селезінки
tyazhkist-u-pidreberyakh       ; тяжкість у підреберях
nyuyuchi-boli-u-pidreberyakh   ; ниючі болі у підреберях
plyamysto-papulozna-vysypka    ; плямисто-папульозна висипка
ponos                          ; понос
nudota                         ; нудота
blyuvannya                     ; блювання
bil-za-grudynoyu               ; біль за грудиною
porushennya-kovtannya          ; порушення ковтання
```

## Приклади аналізів

```lisp
pidvyshchenyy-riven-leykocytiv      ; підвищений рівень лейкоцитів
pidvyshchenyy-riven-limfocytiv      ; підвищений рівень лімфоцитів
atypovi-mononuklearni-klityny       ; атипові мононуклеарні клітини
```

## Рівні ймовірності ВІЛ

1. `дуже-висока` - >= 15 симптомів або всі 5 захворювань
2. `висока` - 10-14 симптомів або мононуклеоз + аналізи або інше захворювання
3. `середня` - 6-9 симптомів або специфічна комбінація
4. `низька` - < 6 симптомів або базові симптоми

## Детальна документація

Дивіться файли:
- `README_LAB2.md` - Повна документація лабораторної роботи
- `decision_tree.md` - Опис двійкового дерева рішень
- `hiv.lisp` - Вихідний код з коментарями

## Порівняння з Prolog

Для порівняння з оригінальною Prolog версією:
```bash
cd ../1/
swipl -s hiv.pl

?- має_вірогідність_віл(людина1, X).
X = висока
```

Обидві реалізації дають ідентичні результати!
