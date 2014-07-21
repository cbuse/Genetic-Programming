;; Partea 1 - regresie simbolică
;; _____________________________

(require racket/list)
(require test-engine/racket-tests)

;; Reprezentarea indivizilor
;; _________________________

;; Un individ este reprezentat printr-o listă (conținut fitness eroare).
;; Conținutul este un AST dependent de problemă, reprezentând:
;; * pentru problema de regresie simbolică (partea 1), corpul unei funcții 
;; * pentru problema de aproximare a unei imagini (partea 2), codul de desenare
;;   a unei imagini.
;; Exemple de conținut:
;; * partea 1: (+ 1 (+ x (* x x)))
;; * partea 2: (above . .)

;; Parametrii generali ai alogritmului evolutiv
;; ____________________________________________

;; Mulțimea de terminali. Se observă prezența variabilei 'x', parametrul
;; funcției căutate.
(define TERMINALS '(0 1 2 x))

;; Mulțimea de funcții. Fiecare funcție este reprezentată sub forma
;; unei perechi (nume . aritate). Aritate = număr de parametri.
(define FUNCTIONS '((+ . 2) (* . 2)))

;; Dimensiunea populației
(define POPULATION-SIZE 50)

;; Adâncimea maximă a AST-urilor populației inițiale
(define DEPTH 6)

;; Numărul maxim de generații de-a lungul cărora se desfășoară evoluția
(define GENERATIONS 50)

;; Pragul de eroare sub care se consideră că s-a obținut o soluție aproximativă
(define EPSILON 0.01)

;; Probabilitatea cu care se alege recombinarea, în defavoarea mutației
(define CROSSOVER-PROBABILITY 0.9)

;; Coeficientul de concizie, care stabilește ponderea cu care este penalizat
;; un candidat în funcție de lungimea sa (lungime mare = penalizare mare)
(define PARSIMONY-COEFFICIENT 0.2)

;; Datele de intrare pentru regresia simbolică
;; ___________________________________________

;; Intervalul de valori pe care le ia 'x': (-1 -0.9 -0.8 ... 0 ... 0.8 0.9 1)
(define RANGE (range -1 1 0.1))

;; Intevalul de valori ale funcției reale, ce se dorește aproximată,
;; și anume x^2 + x + 1, pe intervalul de mai sus
(define SAMPLES (map (λ (x) (+ (* x x) x 1)) RANGE))

;; Evoluție
;; ________

;; Funcția principală, care desfășoară evoluția.
;; Întoarce un individ soluție, sau, în cazul în care nicio soluție
;; nu a fost identificată după epuizarea numărului maxim de generații,
;; cel mai 'fit' individ obținut până atunci.
(define (evolve)
  (let ([initial-population (ramped-half-and-half)])  
    (let loop ([population initial-population]        ;; populația curentă       
               [generation GENERATIONS]               ;; nr. de gen. rămase      
               [the-best (best initial-population)])  ;; cel mai fit, până acum
      (let ([sol (solution population)])
        (cond [(not (eq? sol 'not-found)) sol]  ;; găsirea unei soluții
              [(zero? generation) the-best]     ;; epuizarea generațiilor
              [else (loop (renew population)    ;; continuarea evoluției
                          (- generation 1)
                          (let ([candidate (best population)])
                            (if (better? candidate the-best)
                                candidate
                                the-best)))])))))

;; Inițializarea populației și generarea indivizilor
;; _________________________________________________

;; Întoarce o populație obținută aleator prin metoda 'ramped half and half'
;; (vezi explicațiile din enunț)
(define (ramped-half-and-half)
  (let* ([number-of-groups (- DEPTH 1)]
         [group-size (+ (quotient POPULATION-SIZE
                                  number-of-groups)
                        (sgn (remainder POPULATION-SIZE
                                        number-of-groups)))])  ;; signum
    (let ramped ([counter POPULATION-SIZE]
                 [depth DEPTH])
      (if (zero? counter)
          '()
          (cons (individualize (generate depth (if (even? counter)
                                                   'grow
                                                   'full)))
                (let ([new-counter (- counter 1)])
                  (ramped new-counter
                          (if (zero? (remainder new-counter group-size))
                              (- depth 1)
                              depth))))))))

;; Generează un AST aleator, pe baza adâncimii maxime și a metodei furnizate,
;; 'grow' sau 'full' (vezi enunțul), pornind de la mulțimile de funcții
;; și terminali
(define (generate depth method)
  (let gen ([d depth])
    (if (or (zero? d)
            (and (eq? method 'grow)
                 (zero? (random 2))))
        (choose-from TERMINALS)
        (let ([function-info (choose-from FUNCTIONS)])
          (cons (primitive-name function-info)
                (let repeat ([n (primitive-arity function-info)])
                  (if (zero? n)
                      '()
                      (cons (gen (- d 1))
                            (repeat (- n 1))))))))))



;; Generează un individ de forma (AST fitness eroare), pornind de la un AST
(define (individualize function)
  (make-individual function (properties function)))

;; Fitness
;; _______

;; Calculează informația specifică fiecărui individ în forma unei liste
;; (fitness eroare), pornind de la un AST. Fitness-ul însuși este calculat
;; pe baza erorii de aproximare și a coeficientului de concizie (vezi enunțul).
(define (properties ast)
  (let ([dev (deviation ast)])
    (list (max 0 (- (/ 100 (+ dev 0.1))  ; Adăugăm 0.1 pt a evita împărțirea la 0
                    (* PARSIMONY-COEFFICIENT (size ast))))
          dev)))

;; *** TODO ***
;; Calculează eroarea de aproximare pentru un AST dat
(check-expect (approx? (deviation 2) 17.98) true)
(check-expect (approx? (deviation 'x) 28.7) true)
(check-expect (approx? (deviation '(+ x 1)) 7.7) true)
(check-expect (approx? (deviation '(+ (* x x) 1)) 11) true)

;Calculeaza valorile functiilor de aproximare in fiecare punct din RANGE
(define (deviation-helper ast )
  (map (λ(point) (eval `((λ (x) ,ast) ,point))) RANGE))


(define (deviation ast) 
  (foldl + 0 (map (λ(a b) (abs (- a b))) (deviation-helper ast ) SAMPLES)))


;; *** TODO ***
;; Determină lungimea reprezentării unui AST
(check-expect (size 'x) 1)
(check-expect (size '(+ x y)) 3)
(check-expect (size '(+ x (+ 0 1))) 5)

(define (size ast)
  (if (list? ast)
      (if (empty? ast)
          0
          (if (list? (individual-ast ast))
              (+ (size (individual-probability  ast))(size (individual-ast ast)))
              (+ 1 (size (individual-probability  ast)))))
      
      1))

;; Determină dacă un individ este superior altuia, pe baza fitness-urilor
(define (better? x y)
  (> (individual-fitness x) (individual-fitness y)))

;; Selecție
;; ________

;; Selectează aleator un individ dintr-o populație, cu o probabilitate
;; proporțională cu fitness-ul său
(define (select-random population)
  (select-with (random) population))

;; Selectează un individ dintr-o populație, cu o probabilitatea proporțională
;; cu fitness-ul său, pornind de la o valoare fixată a "zarurilor".
(check-expect (select-with 1/17  '((x 2 0) (y 5 0) (z 10 0))) 'x)
(check-expect (select-with 4/17  '((x 2 0) (y 5 0) (z 10 0))) 'y)
(check-expect (select-with 11/17 '((x 2 0) (y 5 0) (z 10 0))) 'z)

(define (select-with choice population)
  (let search ([intervals (probability-intervals population)])
    (let ([head (car intervals)]
          [tail (cdr intervals)])
      (if (or (null? tail)
              (<= choice (individual-probability head)))
          (individual-ast head)
          (search tail)))))

;; Calculează limitele intervalelor de probabilitate, aferente metodei ruletei
(check-expect (probability-intervals '((x 2 0) (y 5 0) (z 10 0)))
              '((x . 2/17) (y . 7/17) (z . 17/17)))

(define (probability-intervals population)
  (let compute ([acc-probability 0]
                [probabs (probabilities population)])
    (if (null? probabs)
        probabs
        (let* ([individual (car probabs)]
               [new-probability (+ (individual-probability individual)
                                   acc-probability)])
          (cons (make-individual (individual-ast individual)
                                 new-probability)
                (compute new-probability (cdr probabs)))))))

;; Calculează probabilitățile de selecție ale fiecărui individ
;; dintr-o populație, pe baza fitness-ului acestora.
(check-expect (probabilities '((x 2 0) (y 5 0) (z 10 0)))
              '((x . 2/17) (y . 5/17) (z . 10/17)))

(define (probabilities population)
  (let ([fitness-sum (sum (map individual-fitness population))])
    (map (λ (individual)
           (make-individual (individual-ast individual)
                            (/ (individual-fitness individual) fitness-sum)))
         population)))

;; Operatori genetici
;; __________________

;; Construiește populația aferentă unei noi generații, pornind de la populația
;; aferentă celei curente, prin aplicarea operatorilor genetici, cu o anumită
;; probabilitate
(define (renew population)
  (let fill ([size POPULATION-SIZE])
    (if (zero? size)
        '()
        (cons (individualize (let ([choice (random)])
                               (cond [(< choice CROSSOVER-PROBABILITY)
                                      (crossover (select-random population)
                                                 (select-random population))]
                                     [else
                                      (mutate (select-random population))])))
              (fill (- size 1))))))

;; Recombinare
(define (crossover x y)
  (modify-random x (random-subtree y)))

;; Mutație
(define (mutate x)
  (modify-random x (generate (+ 2 (random (- DEPTH 1)))
                             (choose-from '(grow full)))))

;; Determinarea soluției și a celui mai bun individ
;; ________________________________________________

;; Întoarce o soluție din cadrul populației (deviation ~ 0), sau 'not-found
;; dacă aceasta nu există
(check-expect (solution '()) 'not-found)
(check-expect (solution '((x 57 2))) 'not-found)
(check-expect (solution '((x 57 0))) '(x 57 0))
(check-expect (solution '((x 57 0) (y 57 0))) '(x 57 0))
(check-expect (solution '((x 57 2) (y 57 0))) '(y 57 0))

(define (solution population)
  (let search ([pop population])
    (cond [(null? pop) 'not-found]
          [(<= (individual-deviation (car pop)) EPSILON) (car pop)]
          [else (search (cdr pop))])))

;; Determină cel mai bun individ al unei populații, pe baza fitness-ului
(check-expect (best '((x 57 0))) '(x 57 0))
(check-expect (best '((x 58 0) (y 57 0))) '(x 58 0))
(check-expect (best '((x 57 0) (y 58 0))) '(y 58 0))

(define (best population)
  (foldl (λ (x min)
           (if (better? x min) x min))
         (car population)
         (cdr population)))

;; Funcții ajutătoare
;; __________________

;; Întoarce un subarbore aleator al unui AST
(define (random-subtree ast)
  (if (not (list? ast))
      ast
      (let ([choice (choose-from (cons ast (cdr ast)))])
        (if (eq? choice ast)
            ast
            (random-subtree choice)))))

;; Înlocuiește un subarbore aleator al unui AST cu un altul
(define (modify-random ast new)
  (if (not (list? ast))
      new
      (let ([index (random (length ast))])
        (if (zero? index)
            new
            (modify ast index (modify-random (list-ref ast index) new))))))

;; Înlocuiește subarborele de pe poziția 'index' din 'ast' cu 'new'
(check-expect (modify '(10 20 30) 0 'new) '(new 20 30))
(check-expect (modify '(10 20 30) 1 'new) '(10 new 30))
(check-expect (modify '(10 20 30) 2 'new) '(10 20 new))

(define (modify ast index new)
  (if (zero? index)
      (cons new (cdr ast))
      (cons (car ast)
            (modify (cdr ast) (- index 1) new))))

;; Întoarce un element aleator al listei, sau 'nothing dacă lista este vidă
(check-expect (choose-from '()) 'nothing)
(check-expect (choose-from '(0)) 0)

(define (choose-from options)
  (if (null? options)
      'nothing
      (list-ref options (random (length options)))))

;; Calculează suma elementelor unei liste
(check-expect (sum '(1 2 3)) 6)

(define (sum L)
  (foldl + 0 L))

;; Verifică dacă două numere reale sunt aproximativ egale
(define (approx? x y)
  (< (abs (- x y)) 0.001))
       
;; Selectori
;; _________

;; Se preferă ascunderea funcției propriu-zise de acces, 'car', 'cadr' etc.,
;; pentru izolarea reprezentării interne.

;; Funcții pe primitive (funcții și terminali)
(define primitive-name car)
(define primitive-arity cdr)

;; Funcții pe indivizi, ce pot lua următoarele forme în decursul prelucrărilor:
;; * individual = (ast fitness eroare) SAU
;; * individual = (ast . probabilitate).
(define make-individual cons)
(define individual-ast car)
(define individual-fitness cadr)
(define individual-deviation caddr)
(define individual-probability cdr)


;(test)
;(evolve)
