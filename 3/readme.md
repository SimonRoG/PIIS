# ЛАБОРАТОРНА РОБОТА 3

## Побудова на Lisp двійкового дерева рішень діагностичної бази знань

**Виконав:** Семен Прохода ІП-з31

### Завдання до роботи:

**Варіант 13:** "ВІЛ-інфекція"

1.  Описати словесно факти і правила для розроблюваного прототипу предметної області з лабораторної роботи No1, представити можливу ієрархію понять.
2.  Перевести факти і правила в синтаксис мови CLIPS.

### Факти і правила (ієрархія понять) для розроблюваного прототипу "ВІЛ-інфекція"

#### Факти:

-   Симптоми
    -   Мононуклеозу
        -   Підвищена температура
        -   Ангіна
        -   Запалення лімфовузлів
        -   Слабкість
        -   Розбитість
        -   Потовиділення
        -   Головний біль
        -   Зниження апетиту
        -   Порушення сну
        -   Збільшення печінки
        -   Збільшення селезінки
        -   Тяжкість у підребер'ях
        -   Ниючі болі у підребер'ях
        -   Плямисто-папульозна висипка
        -   Понос
    -   Серозного менінгіту та енцефаліту
        -   Нудота
        -   Блювання
    -   Езофагіту
        -   Біль за грудиною
        -   Порушення ковтання
-   Аналізи мононуклеозу
    -   Підвищений рівень лейкоцитів
    -   Підвищений рівень лімфоцитів
    -   Атипові мононуклеарні клітини

#### Правила:

-   Визначення захворювань за симптомами / аналізами
    -   Мононуклеоз
        -   Людина має ангіну
        -   Людина має запалення лімфовузлів
    -   Мононуклеоз
        -   Людина має аналізи: підвищений рівень лейкоцитів, або
        -   Підвищений рівень лімфоцитів, або
        -   Атипові мононуклеарні клітини
    -   Серозний менінгіт або енцефаліт
        -   Нудота
        -   Блювання
    -   Езофагіт
        -   Біль за грудиною
        -   Порушення ковтання
-   Визначення ймовірності ВІЛ-інфекції
    -   Дуже висока
        -   Людина має симптоми мононуклеозу
        -   Людина має аналізи мононуклеозу
        -   Людина має симптоми серозного менінгіту
        -   Людина має симптоми енцефаліту
        -   Людина має симптоми езофагіту
        -   **_АБО_**
        -   Кількість симптомів ВІЛ >= 15
    -   Висока
        -   Людина має симптоми та аналізи мононуклеозу, або
        -   Людина має симптоми серозного менінгіту, або
        -   Людина має симптоми енцефаліту, або
        -   Людина має симптоми езофагіту
        -   **_АБО_**
        -   10 <= кількість симптомів ВІЛ < 15
    -   Середня
        -   Людина має порушення сну
        -   Людина має тяжкість у підребер'ях
        -   Людина має ниючі болі у підребер'ях
        -   Людина має понос
        -   Людина має нудоту
        -   Людина має блювання
        -   **_АБО_**
        -   6 <= кількість симптомів ВІЛ < 10
    -   Низька
        -   Людина має підвищену температуру
        -   Людина має ангіну
        -   Людина має слабкість
        -   Людина має головний біль
        -   **_АБО_**
        -   Кількість симптомів ВІЛ < 6

### Лістниг

#### [`hiv.clp`](./hiv.clp)

```lisp
(deftemplate symptom
  (slot name))

(deftemplate analysis
  (slot name))

(deftemplate disease
  (slot name))

(deftemplate suggests
  (slot symptom)
  (slot disease))

(deftemplate patient
  (multislot symptoms)
  (multislot analyses))

(deftemplate has-symptom
  (slot patient)
  (slot symptom))

(deftemplate has-analysis
  (slot patient)
  (slot analysis))

(deftemplate has-disease
  (slot patient)
  (slot disease))

(deftemplate has-analysis-disease
  (slot patient)
  (slot disease))

(deftemplate k
  (slot patient)
  (slot num))

(deftemplate hiv-probability
  (slot patient)
  (slot probability))

(deffacts symptoms
  (symptom (name fever))
  (symptom (name angina))
  (symptom (name lymph_nodes))
  (symptom (name weakness))
  (symptom (name fatigue))
  (symptom (name sweating))
  (symptom (name headache))
  (symptom (name loss_appetite))
  (symptom (name sleep_disorder))
  (symptom (name liver_enlarged))
  (symptom (name spleen_enlarged))
  (symptom (name heaviness_rhypochondrium))
  (symptom (name dull_pain_rhypochondrium))
  (symptom (name rash))
  (symptom (name diarrhea))
  (symptom (name nausea))
  (symptom (name vomiting))
  (symptom (name retrosternal_pain))
  (symptom (name swallowing_disorder)))

(deffacts analyses
  (analysis (name high_leukocytes))
  (analysis (name high_lymphocytes))
  (analysis (name atypical_mononuclear_cells)))

(deffacts diseases
  (disease (name mononucleosis))
  (disease (name meningitis))
  (disease (name encephalitis))
  (disease (name esophagitis)))

(deffacts symptom-suggests
  (suggests (symptom fever) (disease mononucleosis))
  (suggests (symptom angina) (disease mononucleosis))
  (suggests (symptom lymph_nodes) (disease mononucleosis))
  (suggests (symptom weakness) (disease mononucleosis))
  (suggests (symptom fatigue) (disease mononucleosis))
  (suggests (symptom sweating) (disease mononucleosis))
  (suggests (symptom headache) (disease mononucleosis))
  (suggests (symptom loss_appetite) (disease mononucleosis))
  (suggests (symptom sleep_disorder) (disease mononucleosis))
  (suggests (symptom liver_enlarged) (disease mononucleosis))
  (suggests (symptom spleen_enlarged) (disease mononucleosis))
  (suggests (symptom heaviness_rhypochondrium) (disease mononucleosis))
  (suggests (symptom dull_pain_rhypochondrium) (disease mononucleosis))
  (suggests (symptom rash) (disease mononucleosis))

  (suggests (symptom headache) (disease meningitis))
  (suggests (symptom nausea) (disease meningitis))
  (suggests (symptom vomiting) (disease meningitis))
  (suggests (symptom fever) (disease meningitis))

  (suggests (symptom headache) (disease encephalitis))
  (suggests (symptom nausea) (disease encephalitis))
  (suggests (symptom vomiting) (disease encephalitis))
  (suggests (symptom fever) (disease encephalitis))

  (suggests (symptom retrosternal_pain) (disease esophagitis))
  (suggests (symptom swallowing_disorder) (disease esophagitis)))

(defrule patient-has-symptom
  ?patient <- (patient (symptoms $?before ?symptom $?after))
  =>
  (assert (has-symptom (patient ?patient) (symptom ?symptom))))

(defrule patient-has-analysis
  ?patient <- (patient (analyses $?before ?analysis $?after))
  =>
  (assert (has-analysis (patient ?patient) (analysis ?analysis))))

(defrule has-mono-symptoms
  (has-symptom (patient ?patient) (symptom angina))
  (has-symptom (patient ?patient) (symptom lymph_nodes))
  =>
  (assert (has-disease (patient ?patient) (disease mononucleosis))))

(defrule has-mono-analyses
  (has-analysis
    (patient ?patient)
    (analysis ?analysis&:(or
      (eq ?analysis high_leukocytes)
      (eq ?analysis high_lymphocytes)
      (eq ?analysis atypical_mononuclear_cells))))
  =>
  (assert (has-analysis-disease (patient ?patient) (disease mononucleosis))))

(defrule has-meningitis-symptoms
  (has-symptom (patient ?patient) (symptom nausea))
  (has-symptom (patient ?patient) (symptom vomiting))
  =>
  (assert (has-disease (patient ?patient) (disease meningitis))))

(defrule has-encephalitis-symptoms
  (has-symptom (patient ?patient) (symptom nausea))
  (has-symptom (patient ?patient) (symptom vomiting))
  =>
  (assert (has-disease (patient ?patient) (disease encephalitis))))

(defrule has-esophagitis-symptoms
  (has-symptom (patient ?patient) (symptom retrosternal_pain))
  (has-symptom (patient ?patient) (symptom swallowing_disorder))
  =>
  (assert (has-disease (patient ?patient) (disease esophagitis))))

(defrule countk
  ?patient <- (patient (symptoms ?symptoms) (analyses ?analyses))
  =>
  (bind ?num (length$ ?symptoms))
  (assert (k (patient ?patient) (num ?num))))

(defrule very-high-hiv-probability
  (or
    (and
      (has-disease (patient ?patient) (disease mononucleosis))
      (has-analysis-disease (patient ?patient) (disease mononucleosis))
      (has-disease (patient ?patient) (disease meningitis))
      (has-disease (patient ?patient) (disease encephalitis))
      (has-disease (patient ?patient) (disease esophagitis)))
    (k
      (patient ?patient)
      (num ?num&:(>= ?num 15))))
  =>
  (assert (hiv-probability (patient ?patient) (probability very_high))))

(defrule high-hiv-probability
  (or
    (or
      (and
        (has-disease (patient ?patient) (disease mononucleosis))
        (has-analysis-disease (patient ?patient) (disease mononucleosis)))
      (has-disease (patient ?patient) (disease meningitis))
      (has-disease (patient ?patient) (disease encephalitis))
      (has-disease (patient ?patient) (disease esophagitis)))
    (k
      (patient ?patient)
      (num ?num&:(and
        (< ?num 15)
        (>= ?num 10)))))
  =>
  (assert (hiv-probability (patient ?patient) (probability high))))

(defrule moderate-hiv-probability
  (or
    (and
      (has-symptom (patient ?patient) (symptom sleep_disorder))
      (has-symptom (patient ?patient) (symptom heaviness_rhypochondrium))
      (has-symptom (patient ?patient) (symptom dull_pain_rhypochondrium))
      (has-symptom (patient ?patient) (symptom diarrhea))
      (has-symptom (patient ?patient) (symptom nausea))
      (has-symptom (patient ?patient) (symptom vomiting)))
    (k
      (patient ?patient)
      (num ?num&:(and
        (< ?num 10)
        (>= ?num 6)))))
  =>
  (assert (hiv-probability (patient ?patient) (probability moderate))))

(defrule low-hiv-probability
  (or
    (and
      (has-symptom (patient ?patient) (symptom fever))
      (has-symptom (patient ?patient) (symptom angina))
      (has-symptom (patient ?patient) (symptom weakness))
      (has-symptom (patient ?patient) (symptom headache)))
    (k
      (patient ?patient)
      (num ?num&:(< ?num 6))))
  =>
  (assert (hiv-probability (patient ?patient) (probability low))))

(defglobal ?*patient* = (assert
  (patient
    (symptoms fever angina lymph_nodes diarrhea nausea vomiting retrosternal_pain headache fatigue)
    (analyses high_leukocytes))))

(deffunction diagnosis-report (?patient)
  (bind ?symptoms (fact-slot-value ?patient symptoms))
  (bind ?analyses (fact-slot-value ?patient analyses))
  (bind ?probability (fact-slot-value (nth$ 1 (find-all-facts ((?f hiv-probability)) (eq ?f:patient ?patient))) probability))

  (printout t "Symptoms: " ?symptoms crlf)
  (printout t "Analyses: " ?analyses crlf)
  (printout t "HIV Probability: " ?probability crlf))
```

### Результати роботи програми

Запуск через `clisp`, завантаження файлу `(load hiv.clp)`, виконати усіх правил `(reset)`, `(run)`. Показати усі факти `(facts)`. виклик функції `(diagnosis-report ?*patient*)`

```
         CLIPS (6.4.2 1/14/25)
CLIPS> (load hiv.clp)
%%%%%%%%%%%$$$$************:!
TRUE
CLIPS> (reset)
CLIPS> (run)
CLIPS> (facts)
f-1     (patient (symptoms fever angina lymph_nodes diarrhea nausea vomiting retrosternal_pain headache fatigue) (analyses high_leukocytes))
f-2     (symptom (name fever))
f-3     (symptom (name angina))
f-4     (symptom (name lymph_nodes))
f-5     (symptom (name weakness))
f-6     (symptom (name fatigue))
f-7     (symptom (name sweating))
f-8     (symptom (name headache))
f-9     (symptom (name loss_appetite))
f-10    (symptom (name sleep_disorder))
f-11    (symptom (name liver_enlarged))
f-12    (symptom (name spleen_enlarged))
f-13    (symptom (name heaviness_rhypochondrium))
f-14    (symptom (name dull_pain_rhypochondrium))
f-15    (symptom (name rash))
f-16    (symptom (name diarrhea))
f-17    (symptom (name nausea))
f-18    (symptom (name vomiting))
f-19    (symptom (name retrosternal_pain))
f-20    (symptom (name swallowing_disorder))
f-21    (analysis (name high_leukocytes))
f-22    (analysis (name high_lymphocytes))
f-23    (analysis (name atypical_mononuclear_cells))
f-24    (disease (name mononucleosis))
f-25    (disease (name meningitis))
f-26    (disease (name encephalitis))
f-27    (disease (name esophagitis))
f-28    (suggests (symptom fever) (disease mononucleosis))
f-29    (suggests (symptom angina) (disease mononucleosis))
f-30    (suggests (symptom lymph_nodes) (disease mononucleosis))
f-31    (suggests (symptom weakness) (disease mononucleosis))
f-32    (suggests (symptom fatigue) (disease mononucleosis))
f-33    (suggests (symptom sweating) (disease mononucleosis))
f-34    (suggests (symptom headache) (disease mononucleosis))
f-35    (suggests (symptom loss_appetite) (disease mononucleosis))
f-36    (suggests (symptom sleep_disorder) (disease mononucleosis))
f-37    (suggests (symptom liver_enlarged) (disease mononucleosis))
f-38    (suggests (symptom spleen_enlarged) (disease mononucleosis))
f-39    (suggests (symptom heaviness_rhypochondrium) (disease mononucleosis))
f-40    (suggests (symptom dull_pain_rhypochondrium) (disease mononucleosis))
f-41    (suggests (symptom rash) (disease mononucleosis))
f-42    (suggests (symptom headache) (disease meningitis))
f-43    (suggests (symptom nausea) (disease meningitis))
f-44    (suggests (symptom vomiting) (disease meningitis))
f-45    (suggests (symptom fever) (disease meningitis))
f-46    (suggests (symptom headache) (disease encephalitis))
f-47    (suggests (symptom nausea) (disease encephalitis))
f-48    (suggests (symptom vomiting) (disease encephalitis))
f-49    (suggests (symptom fever) (disease encephalitis))
f-50    (suggests (symptom retrosternal_pain) (disease esophagitis))
f-51    (suggests (symptom swallowing_disorder) (disease esophagitis))
f-52    (has-symptom (patient <Fact-1>) (symptom fever))
f-53    (has-symptom (patient <Fact-1>) (symptom angina))
f-54    (has-symptom (patient <Fact-1>) (symptom lymph_nodes))
f-55    (has-disease (patient <Fact-1>) (disease mononucleosis))
f-56    (has-symptom (patient <Fact-1>) (symptom diarrhea))
f-57    (has-symptom (patient <Fact-1>) (symptom nausea))
f-58    (has-symptom (patient <Fact-1>) (symptom vomiting))
f-59    (has-disease (patient <Fact-1>) (disease meningitis))
f-60    (hiv-probability (patient <Fact-1>) (probability high))
f-61    (has-disease (patient <Fact-1>) (disease encephalitis))
f-62    (has-symptom (patient <Fact-1>) (symptom retrosternal_pain))
f-63    (has-symptom (patient <Fact-1>) (symptom headache))
f-64    (has-symptom (patient <Fact-1>) (symptom fatigue))
f-65    (has-analysis (patient <Fact-1>) (analysis high_leukocytes))
f-66    (has-analysis-disease (patient <Fact-1>) (disease mononucleosis))
For a total of 66 facts.
CLIPS> (diagnosis-report ?*patient*)
Symptoms: (fever angina lymph_nodes diarrhea nausea vomiting retrosternal_pain headache fatigue)
Analyses: (high_leukocytes)
HIV Probability: high
```

### Висновок:
