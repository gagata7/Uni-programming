#lang racket
(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join
)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

;sprawdzanie czy x jest typu "type"
(define (is-type? x type)
  ((eval (string->symbol (string-append (symbol->string type) "?"))) x)
)

;sprawdzanie czy typ wstawianego wiersza sie zgadza ze schematem tabeli
(define (valid-row row cols)
  (cond
    [(and (null? row) (null? cols)) #t] 
    [(or (null? row) (null? cols)) #f] 
    [(not (is-type? (car row) (column-info-type (car cols)))) #f] 
    [else (valid-row (cdr row) (cdr cols))] 
  )
)

; Wstawianie---------------------------------------------------------------------------------------------------
(define (table-insert row tab)
  (if (valid-row row (table-schema tab))
      (table (table-schema tab) (cons row (table-rows tab)))
      (error "error: bad scheme")
  )
)

;znajdz indeks kolumny o szukanej nazwie
(define (col-id szukana tab)
  (index-where (table-schema tab)
    (lambda (sprawdz) (eq? (column-info-name sprawdz) szukana))
  )
)

;zwroc wartosc w konkretnym wierszu i kolumnie
(define (give-value row col tab)
  (list-ref row (col-id col tab))
)

(define (czy-nalezy? x lista)
  (cond
    [(null? lista) #f]
    [(eq? (car lista) x) #t]
    [else (czy-nalezy? x (cdr lista))]
  )
)

;Projekcja----------------------------------------------------------------------------------------------------------------
(define (table-project cols tab) #t
;  (define wybrane ;zawiera wszystkie indeksy kolumn ktore chcemy wypisac
;    (indexes-where (table-schema tab)
;      (lambda (x) (member (column-info-name x) cols)))
;  )
;  (print wybrane)
  ;(define new-schema
    ;(filter (if (null? (member (col-id c tab) wybrane)) #f #t) (table-schema tab))
    ;(define lista '())
    ;(for ([c (table-schema tab)])
    ;  (if (null? (member (col-id c tab) wybrane)) lista
    ;      (append lista c)
    ;  )
    ;)
   ; (print lista)
  ;)
)

;porownywanie wartosci v1 i v2
(define (porownaj-wartosci v1 v2)
  (cond
    [(and (string? v1) (string? v2)) (string<? v1 v2)]
    [(and (symbol? v1) (symbol? v2)) (string<? (symbol->string v1) (symbol->string v2))]
    [(and (number? v1) (number? v2)) (< v1 v2)]
    [(and (boolean? v1) (boolean? v2)) (and (not v1) v2)]
    ;jak sie rozpisze tabelke wartosciowan to dziala
    [else #f]
  )
)

;porownywanie po kolumnach wierszy r1 i r2
(define (porownaj-kolumny r1 r2 cols tab)
  (cond
    [(null? cols) #f]
    [(porownaj-wartosci (give-value r1 (car cols) tab) (give-value r2 (car cols) tab)) #t]
    [(porownaj-wartosci (give-value r2 (car cols) tab) (give-value r1 (car cols) tab)) #f]
    [else (porownaj-kolumny r1 r2 (cdr cols) tab)]
  )
)

; Sortowanie--------------------------------------------------------------------------------------------------------------
(define (table-sort cols tab)
  (table
     (table-schema tab)
     (sort (table-rows tab) (lambda (r1 r2) (porownaj-kolumny r1 r2 cols tab)))
  )
)

; Selekcja------------------------------------------------------------------------------------------------------
(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (suma l1 l2) (remove-duplicates (append l1 l2)))

(define (dopelnienie l reszta)
  (filter (lambda (x) (not (member x l))) reszta)
)

(define (iloczyn l1 l2)
  (dopelnienie (suma (dopelnienie l1 (suma l1 l2)) (dopelnienie l2 (suma l1 l2))) (suma l1 l2))
  ; A & B = ((A | B) \A') \B'
)

(define (table-select-pom form tab)
  (cond
    [(or-f? form) (suma (table-select-pom (or-f-l form) tab) (table-select-pom (or-f-r form) tab))]
    [(not-f? form) (dopelnienie (table-select-pom (not-f-e form) tab) (table-rows tab))]
    [(and-f? form) (iloczyn (table-select-pom (and-f-l form) tab) (table-select-pom (and-f-r form) tab))]
    [(eq-f? form) (filter (lambda (row)
          (equal?
            (give-value row (eq-f-name form) tab)
            (eq-f-val form)
          ))
          (table-rows tab)
    )]
    [(eq2-f? form) (filter (lambda (row)
          (equal?
            (give-value row (eq2-f-name form) tab)
            (give-value row (eq2-f-name form) tab)
          )) (table-rows tab)
    )]
    [(lt-f? form)
     (filter
        (lambda (row)
          (porownaj-wartosci
            (give-value row (lt-f-name form) tab) (lt-f-val form))) (table-rows tab)
    )]
    [else (error "error: invalid form")]
  )
)

(define (table-select form tab)
  (table (table-schema tab) (table-select-pom form tab))
  ;tworze tabele z wynikow z table-select-pom
)


; Zmiana nazwy----------------------------------------------------------------------------------------------------
(define (table-rename col ncol tab)
  (table
   (map (lambda (sprawdz-kolumne)
          (if (eq? (column-info-name sprawdz-kolumne) col)
             (column-info ncol (column-info-type sprawdz-kolumne))
             sprawdz-kolumne
             )
          )
       (table-schema tab)
       )
   (table-rows tab)
  )
)

; Złączenie kartezjańskie----------------------------------------------------------------------
(define (table-cross-join tab1 tab2) #t)

; Złączenie---------------------------------------------------------------------------------------------
(define (table-natural-join tab1 tab2) #t)