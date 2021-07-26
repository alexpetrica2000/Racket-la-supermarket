#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (map (λ(x) (if (= (counter-index x) index) (f x) x)) counters))

(define tt+
  (λ(minute)
   (λ(casa)
     (make-counter (counter-index casa) (+ (counter-tt casa) minute) (counter-et casa) (counter-queue casa)))))

(define et+
  (λ(minute)
   (λ(casa)
     (make-counter (counter-index casa) (counter-tt casa) (+ (counter-et casa) minute) (counter-queue casa)))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
           (make-counter (counter-index C) (+ (counter-tt C) items) (+ (counter-et C) items) (enqueue (cons name items) (counter-queue C)))
           (make-counter (counter-index C) (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C))))))

(define (min-tt-or-et f counters)
  (foldl (λ(x rezultat)
           (if(< (f x) (f rezultat))
              x
              rezultat)) (car counters) (cdr counters)))
(define min-tt
  (λ(counters)
    (cons (counter-index (min-tt-or-et counter-tt counters)) (counter-tt (min-tt-or-et counter-tt counters) )))) ; folosind funcția de mai sus
(define min-et
  (λ(counters)
    (cons (counter-index (min-tt-or-et counter-et counters)) (counter-et (min-tt-or-et counter-et counters))))) ; folosind funcția de mai sus

(define (sum-for-tt counters)
  (foldl (λ(x rezultat)
           (+ (cdr x) rezultat)) 0 counters))
(define (remove-first-from-counter C)
  (if (queue-empty? (dequeue (counter-queue C)))
      (empty-counter (counter-index C))
      (make-counter (counter-index C) (sum-for-tt (append (queue-left (dequeue (counter-queue C))) (queue-right (dequeue (counter-queue C))))) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)) )))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (if (> minutes (counter-et C))
        (empty-counter (counter-index C))
         (make-counter (counter-index C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C)))))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (tt-med counters)
  (/ (foldl (λ(x rezultat) (+ (counter-tt x) rezultat)) 0 counters) (length counters)))
(define (make-changes counter minutes)   ; actualizeaza o casa dupa ce trec minutes unitati de timp
  (if (= 0 minutes)
      counter
      (if (> (counter-et counter) minutes)
          ((pass-time-through-counter minutes) counter)
          (if (queue-empty? (counter-queue counter))
              (make-changes counter 0)
              (make-changes (remove-first-from-counter counter) (- minutes (counter-et counter)))))))

(define (left-clients counters number)   ; trecerea timpului pe case, aici scadem direct numarul trimis  ca parametru pe casa, nu are rost sa irosim memorie
  (map (λ(x)(if(> (counter-et x) number)
               ((pass-time-through-counter number)x)
               (if (queue-empty? (counter-queue x))
                   (if (< (counter-et x) number)
                       (empty-counter (counter-index x))
                       ((pass-time-through-counter number)x))
                   (make-changes (remove-first-from-counter x) (- number (counter-et x)))))) counters))
(define (redefine-queue counters queue-exit minutes loop-minutes) ; trecerea timpului de la 1 pana la minutes cu o unitate (pentru a pastra ordinea de iesire)
  (if (= minutes loop-minutes)
      queue-exit
      (redefine-queue (left-clients counters 1)
                      (foldl (λ(x rezultat)(if (> (counter-et x) 1) rezultat
                                     (if (queue-empty? (counter-queue x))
                                         rezultat
                                         (append rezultat (list (cons (counter-index x) (car (top (counter-queue x)))))) )))
                      queue-exit counters)
                      minutes (add1 loop-minutes))))
  
(define (serve-aux requests fast-counters slow-counters queue-exit)
  (if (null? requests)
      (cons queue-exit (append fast-counters slow-counters) )
       (match (car requests)
        [(list 'delay index minutes)
         (if (> index (length fast-counters))
             (serve-aux (cdr requests) fast-counters (update (tt+ minutes) (update (et+ minutes) slow-counters index) index) queue-exit)
             (serve-aux (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) slow-counters queue-exit))]
        [(list name n-items)
         #:when (not (equal? name 'ensure))
         (if (> n-items ITEMS)
              (serve-aux (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) queue-exit)
              (if (> (car (min-tt (append fast-counters slow-counters))) (length fast-counters))
                  (serve-aux (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) queue-exit)
                  (serve-aux (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters queue-exit)))]
         [(list ensure minutes)
          (if (> (tt-med (append fast-counters slow-counters)) minutes)
              (serve-aux requests fast-counters (append slow-counters (list (empty-counter (+ (length (append fast-counters slow-counters)) 1)))) queue-exit)
              (serve-aux (cdr requests) fast-counters slow-counters queue-exit))]
         [number
          (serve-aux (cdr requests) (left-clients fast-counters number) (left-clients slow-counters number)
            (redefine-queue (append fast-counters slow-counters) queue-exit number 0))]
         )))

  
(define (serve requests fast-counters slow-counters)
  (serve-aux requests fast-counters slow-counters '()))
        
