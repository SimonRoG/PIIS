;;; -*- coding: utf-8 -*-
;;; HIV Infection Diagnosis Expert System in Common Lisp
;;; Реалізація експертної системи діагностики ВІЛ-інфекції на мові Lisp
;;;
;;; Цей код є функціональним еквівалентом Prolog програми hiv.pl
;;; та реалізує систему діагностики на основі симптомів і аналізів пацієнта

;;;=============================================================================
;;; ВИЗНАЧЕННЯ СИМПТОМІВ
;;;=============================================================================

;;; Список всіх можливих симптомів ВІЛ-інфекції
(defparameter *all-symptoms*
  '(pidvyshchena-temperatura      ; підвищена температура
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
    ))

;;; Список можливих аналізів
(defparameter *all-analyses*
  '(pidvyshchenyy-riven-leykocytiv      ; підвищений рівень лейкоцитів
    pidvyshchenyy-riven-limfocytiv      ; підвищений рівень лімфоцитів
    atypovi-mononuklearni-klityny       ; атипові мононуклеарні клітини
    ))

;;;=============================================================================
;;; БАЗА ЗНАНЬ: ВІДПОВІДНІСТЬ СИМПТОМІВ ЗАХВОРЮВАННЯМ
;;;=============================================================================

;;; Симптоми, які нагадують мононуклеоз
(defparameter *mononucleosis-symptoms*
  '(pidvyshchena-temperatura
    angina
    zapalennya-limfovuzliv
    slabkist
    rozbytist
    potovidylennya
    golovnyy-bil
    znyzhennya-apetytu
    porushennya-snu
    zbilshennya-pechinky
    zbilshennya-selezinky
    tyazhkist-u-pidreberyakh
    nyuyuchi-boli-u-pidreberyakh
    plyamysto-papulozna-vysypka
    ponos))

;;; Симптоми, які нагадують серозний менінгіт
(defparameter *meningitis-symptoms*
  '(golovnyy-bil
    nudota
    blyuvannya
    pidvyshchena-temperatura))

;;; Симптоми, які нагадують енцефаліт
(defparameter *encephalitis-symptoms*
  '(golovnyy-bil
    nudota
    blyuvannya
    pidvyshchena-temperatura))

;;; Симптоми, які нагадують езофагіт
(defparameter *esophagitis-symptoms*
  '(bil-za-grudynoyu
    porushennya-kovtannya))

;;;=============================================================================
;;; СТРУКТУРА ДАНИХ ПАЦІЄНТА
;;;=============================================================================

;;; Структура для зберігання інформації про пацієнта
(defstruct patient
  (name nil)          ; ім'я пацієнта
  (symptoms nil)      ; список симптомів
  (analyses nil))     ; список аналізів

;;; Глобальна база пацієнтів
(defparameter *patients* (make-hash-table :test 'equal))

;;;=============================================================================
;;; ФУНКЦІЇ ДЛЯ РОБОТИ З ПАЦІЄНТАМИ
;;;=============================================================================

;;; Додати пацієнта до бази
(defun add-patient (name symptoms analyses)
  "Створює нового пацієнта і додає його до бази"
  (setf (gethash name *patients*)
        (make-patient :name name
                      :symptoms symptoms
                      :analyses analyses)))

;;; Отримати пацієнта з бази
(defun get-patient (name)
  "Повертає пацієнта за ім'ям"
  (gethash name *patients*))

;;; Перевірити, чи має пацієнт симптом
(defun has-symptom-p (patient symptom)
  "Перевіряє, чи має пацієнт вказаний симптом"
  (member symptom (patient-symptoms patient)))

;;; Перевірити, чи має пацієнт аналіз
(defun has-analysis-p (patient analysis)
  "Перевіряє, чи має пацієнт вказаний аналіз"
  (member analysis (patient-analyses patient)))

;;; Підрахувати кількість симптомів у пацієнта
(defun count-symptoms (patient)
  "Повертає кількість симптомів у пацієнта"
  (length (patient-symptoms patient)))

;;;=============================================================================
;;; ДІАГНОСТИЧНІ ФУНКЦІЇ ДЛЯ ВИЯВЛЕННЯ ЗАХВОРЮВАНЬ
;;;=============================================================================

;;; Перевірити наявність симптомів мононуклеозу
(defun has-mononucleosis-symptoms-p (patient)
  "Перевіряє, чи є у пацієнта ключові симптоми мононуклеозу:
   ангіна ТА запалення лімфовузлів"
  (and (has-symptom-p patient 'angina)
       (has-symptom-p patient 'zapalennya-limfovuzliv)))

;;; Перевірити наявність аналізів мононуклеозу
(defun has-mononucleosis-analyses-p (patient)
  "Перевіряє, чи є у пацієнта аналізи мононуклеозу:
   підвищений рівень лейкоцитів АБО лімфоцитів АБО атипові клітини"
  (or (has-analysis-p patient 'pidvyshchenyy-riven-leykocytiv)
      (has-analysis-p patient 'pidvyshchenyy-riven-limfocytiv)
      (has-analysis-p patient 'atypovi-mononuklearni-klityny)))

;;; Перевірити наявність симптомів серозного менінгіту
(defun has-meningitis-symptoms-p (patient)
  "Перевіряє, чи є у пацієнта ключові симптоми серозного менінгіту:
   нудота ТА блювання"
  (and (has-symptom-p patient 'nudota)
       (has-symptom-p patient 'blyuvannya)))

;;; Перевірити наявність симптомів енцефаліту
(defun has-encephalitis-symptoms-p (patient)
  "Перевіряє, чи є у пацієнта ключові симптоми енцефаліту:
   нудота ТА блювання"
  (and (has-symptom-p patient 'nudota)
       (has-symptom-p patient 'blyuvannya)))

;;; Перевірити наявність симптомів езофагіту
(defun has-esophagitis-symptoms-p (patient)
  "Перевіряє, чи є у пацієнта ключові симптоми езофагіту:
   біль за грудиною ТА порушення ковтання"
  (and (has-symptom-p patient 'bil-za-grudynoyu)
       (has-symptom-p patient 'porushennya-kovtannya)))

;;;=============================================================================
;;; ВИЗНАЧЕННЯ ЙМОВІРНОСТІ ВІЛ-ІНФЕКЦІЇ
;;;=============================================================================

;;; Перевірка на дуже високу ймовірність ВІЛ
(defun has-very-high-hiv-probability-p (patient)
  "Дуже висока ймовірність ВІЛ, якщо:
   1) Кількість симптомів >= 15, АБО
   2) Є всі захворювання: мононуклеоз + аналізи + менінгіт + енцефаліт + езофагіт"
  (let ((symptom-count (count-symptoms patient)))
    (or (>= symptom-count 15)
        (and (has-mononucleosis-symptoms-p patient)
             (has-mononucleosis-analyses-p patient)
             (has-meningitis-symptoms-p patient)
             (has-encephalitis-symptoms-p patient)
             (has-esophagitis-symptoms-p patient)))))

;;; Перевірка на високу ймовірність ВІЛ
(defun has-high-hiv-probability-p (patient)
  "Висока ймовірність ВІЛ, якщо:
   1) 10 <= кількість симптомів < 15, АБО
   2) Є хоча б одне: (мононуклеоз + аналізи) АБО менінгіт АБО енцефаліт АБО езофагіт"
  (let ((symptom-count (count-symptoms patient)))
    (or (and (>= symptom-count 10)
             (< symptom-count 15))
        (and (has-mononucleosis-symptoms-p patient)
             (has-mononucleosis-analyses-p patient))
        (has-meningitis-symptoms-p patient)
        (has-encephalitis-symptoms-p patient)
        (has-esophagitis-symptoms-p patient))))

;;; Перевірка на середню ймовірність ВІЛ
(defun has-medium-hiv-probability-p (patient)
  "Середня ймовірність ВІЛ, якщо:
   1) 6 <= кількість симптомів < 10, АБО
   2) Є всі специфічні симптоми середнього рівня"
  (let ((symptom-count (count-symptoms patient)))
    (or (and (>= symptom-count 6)
             (< symptom-count 10))
        (and (has-symptom-p patient 'porushennya-snu)
             (has-symptom-p patient 'tyazhkist-u-pidreberyakh)
             (has-symptom-p patient 'nyuyuchi-boli-u-pidreberyakh)
             (has-symptom-p patient 'ponos)
             (has-symptom-p patient 'nudota)
             (has-symptom-p patient 'blyuvannya)))))

;;; Перевірка на низьку ймовірність ВІЛ
(defun has-low-hiv-probability-p (patient)
  "Низька ймовірність ВІЛ, якщо:
   1) Кількість симптомів < 6, АБО
   2) Є всі базові симптоми, АБО
   3) За замовчуванням (якщо не підходить під інші категорії)"
  (let ((symptom-count (count-symptoms patient)))
    (or (< symptom-count 6)
        (and (has-symptom-p patient 'pidvyshchena-temperatura)
             (has-symptom-p patient 'angina)
             (has-symptom-p patient 'slabkist)
             (has-symptom-p patient 'golovnyy-bil)))))

;;; Головна функція визначення ймовірності ВІЛ
(defun diagnose-hiv-probability (patient)
  "Визначає рівень ймовірності ВІЛ-інфекції у пацієнта.
   Повертає: дуже-висока, висока, середня або низька"
  (cond ((has-very-high-hiv-probability-p patient) 'дуже-висока)
        ((has-high-hiv-probability-p patient) 'висока)
        ((has-medium-hiv-probability-p patient) 'середня)
        (t 'низька)))

;;;=============================================================================
;;; ДОПОМІЖНІ ФУНКЦІЇ ДЛЯ ВИВЕДЕННЯ ІНФОРМАЦІЇ
;;;=============================================================================

;;; Вивести інформацію про пацієнта
(defun print-patient-info (patient)
  "Виводить детальну інформацію про пацієнта"
  (format t "~%=== Інформація про пацієнта: ~A ===~%" (patient-name patient))
  (format t "Симптоми (~D): ~{~A~^, ~}~%" 
          (count-symptoms patient)
          (patient-symptoms patient))
  (format t "Аналізи: ~{~A~^, ~}~%" (patient-analyses patient)))

;;; Вивести діагноз
(defun print-diagnosis (patient)
  "Виводить повний діагноз пацієнта"
  (print-patient-info patient)
  (format t "~%--- Діагностика ---~%")
  (format t "Мононуклеоз (симптоми): ~A~%" 
          (if (has-mononucleosis-symptoms-p patient) "ТАК" "НІ"))
  (format t "Мононуклеоз (аналізи): ~A~%" 
          (if (has-mononucleosis-analyses-p patient) "ТАК" "НІ"))
  (format t "Серозний менінгіт: ~A~%" 
          (if (has-meningitis-symptoms-p patient) "ТАК" "НІ"))
  (format t "Енцефаліт: ~A~%" 
          (if (has-encephalitis-symptoms-p patient) "ТАК" "НІ"))
  (format t "Езофагіт: ~A~%" 
          (if (has-esophagitis-symptoms-p patient) "ТАК" "НІ"))
  (format t "~%>>> ЙМОВІРНІСТЬ ВІЛ-ІНФЕКЦІЇ: ~A <<<~%~%" 
          (diagnose-hiv-probability patient)))

;;; Вивести на що нагадує симптом
(defun what-disease-does-symptom-suggest (symptom)
  "Визначає, на яке захворювання вказує симптом"
  (let ((diseases '()))
    (when (member symptom *mononucleosis-symptoms*)
      (push 'мононуклеоз diseases))
    (when (member symptom *meningitis-symptoms*)
      (push 'серозний-менінгіт diseases))
    (when (member symptom *encephalitis-symptoms*)
      (push 'енцефаліт diseases))
    (when (member symptom *esophagitis-symptoms*)
      (push 'езофагіт diseases))
    diseases))

;;;=============================================================================
;;; ІНТЕРАКТИВНА СИСТЕМА ЗАПИТІВ
;;;=============================================================================

;;; Запитати симптоми у користувача
(defun ask-symptoms ()
  "Інтерактивно запитує у користувача про наявність симптомів"
  (format t "~%Будь ласка, введіть симптоми пацієнта~%")
  (format t "Доступні симптоми:~%")
  (loop for i from 1
        for symptom in *all-symptoms*
        do (format t "~2D. ~A~%" i symptom))
  (format t "~%Введіть номери симптомів через пробіл (або 'done' для завершення): ")
  (finish-output)
  (let ((input (read-line)))
    (if (string-equal input "done")
        nil
        (let ((numbers (mapcar #'parse-integer 
                               (remove-if #'(lambda (s) (string= s ""))
                                         (split-string input #\Space)))))
          (mapcar #'(lambda (n) (nth (1- n) *all-symptoms*)) numbers)))))

;;; Запитати аналізи у користувача
(defun ask-analyses ()
  "Інтерактивно запитує у користувача про наявність аналізів"
  (format t "~%Доступні аналізи:~%")
  (loop for i from 1
        for analysis in *all-analyses*
        do (format t "~2D. ~A~%" i analysis))
  (format t "~%Введіть номери аналізів через пробіл (або 'done' для пропуску): ")
  (finish-output)
  (let ((input (read-line)))
    (if (string-equal input "done")
        nil
        (let ((numbers (mapcar #'parse-integer 
                               (remove-if #'(lambda (s) (string= s ""))
                                         (split-string input #\Space)))))
          (mapcar #'(lambda (n) (nth (1- n) *all-analyses*)) numbers)))))

;;; Допоміжна функція для розбиття рядка
(defun split-string (string delimiter)
  "Розбиває рядок на підрядки за роздільником"
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
          do (progn
               (push (subseq string start i) result)
               (setf start (1+ i))))
    (push (subseq string start) result)
    (nreverse result)))

;;; Інтерактивна діагностика
(defun interactive-diagnosis ()
  "Запускає інтерактивну сесію діагностики"
  (format t "~%=== СИСТЕМА ДІАГНОСТИКИ ВІЛ-ІНФЕКЦІЇ ===~%")
  (format t "Введіть ім'я пацієнта: ")
  (finish-output)
  (let* ((name (read-line))
         (symptoms (ask-symptoms))
         (analyses (ask-analyses))
         (patient (make-patient :name name
                               :symptoms symptoms
                               :analyses analyses)))
    (print-diagnosis patient)
    patient))

;;;=============================================================================
;;; ТЕСТОВІ ДАНІ
;;;=============================================================================

;;; Приклад пацієнта з Prolog програми
(defun create-test-patient-1 ()
  "Створює тестового пацієнта людина1 з оригінальної Prolog програми"
  (add-patient "людина1"
               '(pidvyshchena-temperatura
                 angina
                 zapalennya-limfovuzliv
                 ponos
                 nudota
                 blyuvannya
                 bil-za-grudynoyu
                 golovnyy-bil
                 rozbytist)
               '(pidvyshchenyy-riven-leykocytiv)))

;;; Створити пацієнта з дуже високою ймовірністю
(defun create-test-patient-2 ()
  "Створює тестового пацієнта з дуже високою ймовірністю ВІЛ"
  (add-patient "людина2"
               '(pidvyshchena-temperatura
                 angina
                 zapalennya-limfovuzliv
                 slabkist
                 rozbytist
                 potovidylennya
                 golovnyy-bil
                 znyzhennya-apetytu
                 porushennya-snu
                 zbilshennya-pechinky
                 zbilshennya-selezinky
                 tyazhkist-u-pidreberyakh
                 nyuyuchi-boli-u-pidreberyakh
                 plyamysto-papulozna-vysypka
                 ponos
                 nudota
                 blyuvannya
                 bil-za-grudynoyu
                 porushennya-kovtannya)
               '(pidvyshchenyy-riven-leykocytiv
                 atypovi-mononuklearni-klityny)))

;;; Створити пацієнта з низькою ймовірністю
(defun create-test-patient-3 ()
  "Створює тестового пацієнта з низькою ймовірністю ВІЛ"
  (add-patient "людина3"
               '(pidvyshchena-temperatura
                 angina
                 slabkist
                 golovnyy-bil)
               '()))

;;;=============================================================================
;;; ДЕМОНСТРАЦІЯ РОБОТИ СИСТЕМИ
;;;=============================================================================

(defun run-demo ()
  "Запускає демонстрацію роботи системи з тестовими пацієнтами"
  (format t "~%~%===============================================~%")
  (format t "ДЕМОНСТРАЦІЯ РОБОТИ СИСТЕМИ ДІАГНОСТИКИ ВІЛ~%")
  (format t "===============================================~%~%")
  
  ;; Створюємо тестових пацієнтів
  (create-test-patient-1)
  (create-test-patient-2)
  (create-test-patient-3)
  
  ;; Діагностуємо кожного пацієнта
  (format t "~%>>> ТЕСТОВИЙ ПАЦІЄНТ 1 (з Prolog програми) <<<")
  (print-diagnosis (get-patient "людина1"))
  
  (format t "~%~%>>> ТЕСТОВИЙ ПАЦІЄНТ 2 (дуже висока ймовірність) <<<")
  (print-diagnosis (get-patient "людина2"))
  
  (format t "~%~%>>> ТЕСТОВИЙ ПАЦІЄНТ 3 (низька ймовірність) <<<")
  (print-diagnosis (get-patient "людина3"))
  
  (format t "~%===============================================~%")
  (format t "Демонстрація завершена~%")
  (format t "===============================================~%~%"))

;;;=============================================================================
;;; ТОЧКА ВХОДУ
;;;=============================================================================

;;; Вивести інструкції для користувача
(defun print-instructions ()
  "Виводить інструкції для роботи з системою"
  (format t "~%=== ІНСТРУКЦІЇ ===~%")
  (format t "1. Для запуску демонстрації: (run-demo)~%")
  (format t "2. Для інтерактивної діагностики: (interactive-diagnosis)~%")
  (format t "3. Для діагностики існуючого пацієнта: (print-diagnosis (get-patient \"ім'я\"))~%")
  (format t "4. Перегляд інформації про симптом: (what-disease-does-symptom-suggest 'назва-симптому)~%")
  (format t "~%"))

;;; Автоматичний запуск при завантаженні
(format t "~%~%")
(format t "***************************************************~%")
(format t "*  СИСТЕМА ДІАГНОСТИКИ ВІЛ-ІНФЕКЦІЇ (Lisp)      *~%")
(format t "*  HIV Infection Diagnosis Expert System         *~%")
(format t "***************************************************~%")
(print-instructions)

;;; Кінець файлу
