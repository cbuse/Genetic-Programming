;; Partea 2 - Generarea imaginilor
;; _______________________________

(load "symbolic-regression.rkt")
(require 2htdp/image)
(require (lib "trace.ss"))

(define POPULATION-SIZE 100)
(define EPSILON 0.01)

;; Probabilitatea cu care se alege recombinarea, în defavoarea mutației
(define CROSSOVER-PROBABILITY 0.9)

;; Coeficientul de concizie, care stabilește ponderea cu care este penalizat
;; un candidat în funcție de lungimea sa (lungime mare = penalizare mare)
(define PARSIMONY-COEFFICIENT 0.00002)

;; Adâncimea maximă a AST-urilor populației inițiale
(define DEPTH 6)

;; Numărul maxim de generații de-a lungul cărora se desfășoară evoluția
(define GENERATIONS 500)
;; Un terminal este reprezentat în forma unei liste (obiect tip)
(define TERMINALS '((. image) (. image) (. image) (. image)
                    (. image) (. image) (90 360) (80 180) (1.0 2.0)
                    (. image) (. image)))



;; O funcție este reprezentată în forma unei liste
;; (nume tip-întors (tip-param-1 ... tip-param-n))
(define FUNCTIONS '((above image (image image))
                    (rotate image (360 image))
                    (scale image (2.0 image))
                     
                    (rotate image (180 image))
                    (overlay image (image image))
                    (beside image (image image))))

;; Posibilă imagine model
(define SAMPLE .)


(define WIDTH (image-width SAMPLE))
(define HEIGHT (image-height SAMPLE))

;pentru a compara imagini care nu au aceeasi marime, le scalez la marimea lui SAMPLE

(define BACKGROUND (rectangle WIDTH HEIGHT "outline" "white"))


;liste cu culorile rosu, verde si albastru ale lui SAMPLE

(define RED (map color-red (image->color-list SAMPLE)))
(define GREEN (map color-green (image->color-list SAMPLE)))
(define BLUE (map color-blue (image->color-list SAMPLE)))


;plasarea unei imagini in centrul de greutate al lui SAMPLE 
(define (cropped-image img)
  (place-image img (/ WIDTH 2) (/ HEIGHT 2 ) BACKGROUND))


(define (get-red-sum img)
 (foldl + 0 (map (λ(a b) (* (- a b) (- a b))) 
                   (map color-red  (image->color-list  img)) RED)))
(define (get-green-sum img)
 (foldl + 0 (map (λ(a b) (* (- a b) (- a b))) 
                   (map color-green  (image->color-list  img)) GREEN)))
(define (get-blue-sum img)
 (foldl + 0 (map (λ(a b) (* (- a b) (- a b))) 
                   (map color-blue  (image->color-list  img)) BLUE)))



(define (deviation-help img)
  (if (and (eq? (image-width img) WIDTH)
           (eq? (image-height img) HEIGHT))
      (+ (get-red-sum img) (get-green-sum img) (get-blue-sum img))
      (+ (get-red-sum (cropped-image img))
         (get-green-sum (cropped-image img))
         (get-blue-sum (cropped-image img))))) 
   
(define (deviation ast)
  (deviation-help (eval ast)))

 
;generate ar functiona si cu cu un set de functii care intorc acelasi tip si
;au 2 parametri


;tipul comun intors de toate functiile
(define ftype (cadr (car FUNCTIONS)))

(define (generate depth method)
  
  (let gen ([d depth])
           
    (if (or (or (zero? d)
                    (and (eq? method 'grow)
                                           (zero? (random 2)))))
         ;daca sunt intr-o frunza aleg obligatoriu terminal imagine       
        (car (choose-from (filter  (λ(l)(equal? ftype (cadr l))) TERMINALS)))
        
        (let ([function-info (choose-from FUNCTIONS)])
          (cons (primitive-name function-info)
                (let repeat ([list-types (caddr function-info)])
                  (if (empty? list-types)
                      '()  
                      ;daca parametrul functiei nu e de tip imagine
                      (if  (not (equal? (car list-types) ftype))
                           
                         (list (car (choose-from (filter  (λ(l)(equal? (car list-types) (cadr l))) TERMINALS)))
                                 (car (choose-from (filter  (λ(l)(equal? ftype (cadr l))) TERMINALS))))
        
                         (cons (gen (- d 1))
                            (repeat (cdr list-types)))))))))))



;returneaza tipul unui ast
(define (type individual)
  ;daca ast-ul e TERMINAL
  (if  (list? (member individual (map car TERMINALS)))
       (cadr (car (filter  (λ(l)(equal? individual (car l))) TERMINALS)))
       ;daca ast-ul nu e TERMINAL
       (cadr (car (filter  (λ(l)(equal? (car individual) (car l))) FUNCTIONS)))))

(define (modify ast index new)
  (if (zero? index) 
      ;compar tipul arborelui pe care il inserez(new) cu tipul nodului unde il voi insera  
      (if (equal? (type new) (type (car ast)))
          (cons new (cdr ast))
          ast)
      (cons (car ast)
            (modify (cdr ast) (- index 1) new))))

(define (modify-random ast new)
  (if (not (list? ast))
      new
      (let ([index (random (length ast))])
        (if (zero? index)
           (if (equal? (type new) (type  ast))               
            new
            ast)
            (modify ast index (modify-random (list-ref ast index) new))))))

;;cel mai evoluat individ
;(eval (car (evolve)))



