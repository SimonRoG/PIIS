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
