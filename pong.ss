;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pong) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "world.ss" "teachpack" "htdp")))))

(define-struct pelota
  (x y sx sy pic))

(define-struct paddle
  (x y pic))

(define-struct world
  (pc npc                         ; player characters, non player characters, por ahora uno y uno
     time 
;     chances 
     escena
     fin))

;; Muestro todos los caracteres
(define (show-all w)
  (let ((paleta (world-pc w))
        (pelota (world-npc w)))
    (place-image (pelota-pic pelota)
                 (pelota-x pelota)
                 (pelota-y pelota)
                 (place-image (paddle-pic paleta)
                              (paddle-x paleta)
                              (paddle-y paleta)
                              (place-image 
                               (overlay
                                (text (number->string (world-time w)) 10 'blue)
                                (move-pinhole
                                 (text (number->string (pelota-x (world-npc w))) 10 'blue)
                                 0
                                 -15)
                                (move-pinhole
                                 (text (number->string (paddle-x (world-pc w))) 10 'blue)
                                 0
                                 -30))
                               30 10
                               (world-escena w))))))

;; Manejamos la paleta
(define (mouse-move w mx my m-event)
  (let ((paleta (world-pc w)))
    (begin
      (set-paddle-x! paleta 
                    (cond ((and (< mx 457)
                                 (> mx 8 )) mx)
                           (else 457)))
      w)))
    
(define (pegale p paleta)
  (let* ((ball-x (pelota-x p))
         (ball-y (pelota-y p))
         (ball-sx (pelota-sx p))
         (ball-sy (pelota-sy p))
         (padd-x (paddle-x paleta))
         (padd-y (paddle-y paleta))
         (D (- (+ padd-x 17)
                    (+ ball-x
                       ball-sx)))
         (ball-nx (+ ball-x ball-sx))
         (ball-ny (+ ball-y ball-sy)))
    (cond ((< (abs D) 17) (begin
                            (set-pelota-sy! p -8)
                            (set-pelota-sx! p (/ (abs D) 2))
                            p)) 
          (else (begin
                  (set-pelota-x! p ball-nx)
                  (set-pelota-y! p ball-ny)
                  p)))))

(define (mover-pelota p paleta)
  (let ((posx (pelota-x p))
        (posy (pelota-y p))
        (sx (pelota-sx p))
        (sy (pelota-sy p)))
    (cond ((or (< 492 (+ posx sx))
               (> 8 (+ posx sx))) (begin
                                   (set-pelota-sx! p (* -1  sx))
                                   (set-pelota-pic! p (circle 10 'solid 'red))
                                   p))
          ((> 8 (+ posy sy)) (begin
                                   (set-pelota-sy! p (* -1  sy))
                                   (set-pelota-pic! p (circle 10 'solid 'red))
                                   p))
          ((< 570 (+ posy sy)) (pegale p paleta))
          (else 
           (begin
             (set-pelota-x! p (+ posx sx))
             (set-pelota-y! p (+ posy sy))
             (set-pelota-pic! p (circle 10 'solid 'green))
             p)))))

(define (actualizar w)
  (let ((pelota (world-npc w)))
    (begin
      (set-world-time! w (+ 7 (world-time w)))
      (set-world-npc! w (mover-pelota pelota (world-pc w)))
    w)))

(define (se-termina? w)
  (let ((pelota (world-npc w)))
    (cond ((or (world-fin w)
               (> (pelota-y pelota) 590)) #t)
          (else #f))))

(define (handle-key w k)
  (begin
    (set-world-fin! w #t)
    w))

;; Main
(define fondo
  (place-image 
   (nw:rectangle 500 8 'solid 'black)
   0 0 
   (place-image 
    (nw:rectangle 8 600 'solid 'black)
    0 0
    (place-image (nw:rectangle 8 600 'solid 'black)
                 492 0
                 (empty-scene 500 600)))))

(big-bang 500 600 1/30
          (make-world (make-paddle 240 580 (nw:rectangle 35 10 'solid 'blue))
                      (make-pelota 240 25 8 8 (circle 10 'solid 'green))
                      0
                      fondo
                      #f))

(stop-when se-termina?)
(on-key-event handle-key)
(on-redraw show-all)
(on-mouse-event mouse-move)
(on-tick-event actualizar)



  
