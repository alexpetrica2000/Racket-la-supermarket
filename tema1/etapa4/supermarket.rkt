#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue state) #:transparent)

; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue



(define (empty-counter index)
  (make-counter index 0 0 empty-queue 0))

(define (update f counters index)
  (map (λ(x) (if (= (counter-index x) index) (f x) x)) counters))

(define tt+
  (λ(minute)
   (λ(casa)
     (make-counter (counter-index casa) (+ (counter-tt casa) minute) (counter-et casa) (counter-queue casa) (counter-state casa)))))

(define et+
  (λ(minute)
   (λ(casa)
     (make-counter (counter-index casa) (counter-tt casa) (+ (counter-et casa) minute) (counter-queue casa) (counter-state casa)))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
           (make-counter (counter-index C) (+ (counter-tt C) items) (+ (counter-et C) items) (enqueue (cons name items) (counter-queue C)) (counter-state C))
           (make-counter (counter-index C) (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C)) (counter-state C)))))

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

(define (stream-take s n)
  (cond ((zero? n) '())
        ((stream-empty? s) '())
        (else (cons (stream-first s)
                    (stream-take (stream-rest s) (- n 1))))))
(define (list->stream L)
  (if (null? L)
      empty-stream
  (stream-cons (car L) (list->stream (cdr L)))))
(define (sum-for-tt counters)
  (foldl (λ(x rezultat)
           (+ (cdr x) rezultat)) 0 counters))
(define (remove-first-from-counter C)
  (if (queue-empty? (dequeue (counter-queue C)))
      (make-counter (counter-index C) 0 0 empty-queue (counter-state C))
      (make-counter (counter-index C) (sum-for-tt (append (stream-take (queue-left (dequeue (counter-queue C))) (stream-length (queue-left (dequeue (counter-queue C))))) (queue-right (dequeue (counter-queue C))))) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)) (counter-state C) )))
(define (pass-time-through-counter minutes)
  (λ (C)
    (if (> minutes (counter-et C))
        (make-counter (counter-index C) 0 0 empty-queue (counter-state C))
         (make-counter (counter-index C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C) (counter-state C)))))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
;FUNCTII NOI
(define (close-counter index counters)
  (map (λ(x)(if (= (counter-index x) index) (make-counter (counter-index x) (counter-tt x) (counter-et x) (counter-queue x) 1) x)) counters))
(define (opened-counters counters)
  (filter (λ(x)(= 0 (counter-state x))) counters))
;----------------------------------------
(define (tt-med counters)
  (/ (foldl (λ(x rezultat) (+ (counter-tt x) rezultat)) 0 (opened-counters counters)) (length (opened-counters counters))))
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
                       (make-counter (counter-index x) 0 0 empty-queue (counter-state x))
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
      (cons queue-exit (foldr (λ(x rezultat)(if (queue-empty? (counter-queue x)) rezultat (append (list (cons (counter-index x) (counter-queue x))) rezultat))) '() (append fast-counters slow-counters)))
       (match (car requests)
        [(list 'delay index minutes)
         (if (> index (length fast-counters))
             (serve-aux (cdr requests) fast-counters (update (tt+ minutes) (update (et+ minutes) slow-counters index) index) queue-exit)
             (serve-aux (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) slow-counters queue-exit))]
         [(list 'close number)
          (serve-aux (cdr requests) (close-counter number fast-counters) (close-counter number slow-counters) queue-exit)]
         [(list name n-items)
         #:when (not (equal? name 'ensure))
         (if (> n-items ITEMS)
              (serve-aux (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt (opened-counters slow-counters)))) queue-exit)
              (if (> (car (min-tt (opened-counters (append fast-counters slow-counters)))) (length fast-counters))
                  (serve-aux (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt (opened-counters slow-counters)))) queue-exit)
                  (serve-aux (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (opened-counters fast-counters)))) slow-counters queue-exit)))]
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