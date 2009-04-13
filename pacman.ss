;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pacman) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "world.ss" "teachpack" "htdp")))))

;; La pantalla
(define HEIGHT 500)
(define WIDTH  500)
(define SPEED 5)
(define background (empty-scene WIDTH HEIGHT))

;; Mapa
(define mapa
  '("111111111111111111111111111111111111111111111111111111111111"
    "1..........................................................1"
    "1.111.1111111                                              1"
    "1.111.1111111                                              1"
    "1.....1  1  1                                              1"    
    "1     1  1  11111 11111                                    1"
    "1     1  1  1         1                                    1"
    "1     1  1  1 1     1 1                                    1"
    "1     1  1  1         1                                    1"    
    "1     1  1  11111111111                                    1"
    "1                                                          1"
    "1                                                          1"
    "1                                                          1"    
    "1                                                          1"
    "1                                                          1"
    "1                                                          1"
    "1                                                          1"    
    "1                                                          1"
    "1                                                          1"
    "1                                                          1"
    "1                                                          1"    
    "1                                                          1"
    "1                                                          1"
    "1                                                          1"
    "1                                                          1"    
    "111111111111111111111111111111111111111111111111111111111111"))

(define pared-solida (nw:rectangle 10 10 'solid 'blue))
(define espacio-vacio (nw:rectangle 10 10 'solid 'white))

(define (agregar-fila-a imagen-mapa lista-filas)
  (cond ((empty? lista-filas) (reverse imagen-mapa))
        (else
         (agregar-fila-a 
          (cons
           (map (lambda (block)
                  (cond ((eq? block #\1) pared-solida)
                        (else espacio-vacio)))
                (string->list (car lista-filas)))
           imagen-mapa)
          (cdr lista-filas)))))

(define (map-image)
  (agregar-fila-a '() mapa))

   
;; Los jugadores
(define-struct player
  (x y sx sy face points update-func))

;; AI
(define (valid-pos? x y)
  (and (< x WIDTH) (> x 0)
       (< y HEIGHT) (> y 0)))

(define (bad-ai w p)
  (let ((x (player-x p))
        (y (player-y p))
        (sx (player-sx p))
        (sy (player-sy p)))
    (cond ((valid-pos? (+ x sx) (+ y sy)) w)
          (else (begin
                  (set-player-sx! p (* -1 sx))
                  (set-player-sy! p (* -1 sy))
                  w)))))
                  
;; El mundo
(define-struct world
  (jugadores escena final))

(define (place-player e ps)
  (cond ((empty? ps) e)
        (else (let ((p (car ps)))
                (place-player (place-image (player-face p)
                                           (player-x p)
                                           (player-y p)
                                           e)
                              (cdr ps))))))

(define (place-all w)
  (let ((escena (world-escena w))
        (human (first (world-jugadores w)))
        (redy (second (world-jugadores w))))
    (place-player escena
                  (world-jugadores w))))

(define (procesar-tecla w tecla)
  (let ((human (first (world-jugadores w))))
    (cond ((eq? tecla 'up)
           (begin 
             (set-player-sx! human 0)
             (set-player-sy! human (- 0 SPEED))
             w))
          ((eq? tecla 'down)
           (begin
             (set-player-sx! human 0)
             (set-player-sy! human SPEED)
             w))
          ((eq? tecla 'right)
           (begin
             (set-player-sx! human SPEED)
             (set-player-sy! human 0)
             w))
          ((eq? tecla 'left)
           (begin
             (set-player-sx! human (- 0 SPEED))
             (set-player-sy! human 0)
             w))
          ((eq? tecla 'escape)
           (set-world-final! w #t))
          (else w))))

(define (actualizar-jugador w ps)
  (cond ((empty? ps) w)
        (else (let ((p (car ps)))
                (begin
                  ((player-update-func p) w p)
                  (set-player-x! p (+ (player-x p) (player-sx p)))
                  (set-player-y! p (+ (player-y p) (player-sy p)))
                  (actualizar-jugador w (cdr ps)))))))
                
(define (actualizar-todos w)
  (actualizar-jugador w (world-jugadores w)))

(define (se-termino? w)
  (world-final w))

(define human (make-player 10 10 0 0 (circle 10 'solid 'yellow) 0 bad-ai))
(define redy (make-player 50 30 SPEED 0 (circle 10 'solid 'red) 0 bad-ai))
(define pinky (make-player 50 30 0 SPEED (circle 10 'solid 'pink) 0 bad-ai))
(define bluey (make-player 50 30 (- 0 SPEED) 0 (circle 10 'solid 'blue) 0 bad-ai))
(define greeny (make-player 50 30 0 (- 0 SPEED) (circle 10 'solid 'green) 0 bad-ai))

(big-bang WIDTH HEIGHT 1/30 
          (make-world (list human redy pinky bluey greeny) background #f))
(on-redraw place-all)
(on-tick-event actualizar-todos)
(on-key-event procesar-tecla)
(stop-when se-termino?)

