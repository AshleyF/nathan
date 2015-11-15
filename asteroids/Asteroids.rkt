#lang racket

(require math lang/posn racket/generator)

(define (translation-matrix x y)
  (matrix [[1 0 x]
           [0 1 y]
           [0 0 1]]))

(define (scale-matrix w h)
  (matrix [[w 0 0]
           [0 h 0]
           [0 0 1]]))

(define (shear-matrix x y)
  (matrix [[1 x 0]
           [y 1 0]
           [0 0 1]]))

(define (reflection-matrix x y)
  (matrix [[y 0 0]
           [0 x 0]
           [0 0 1]]))

(define (rotation-matrix t)
  (let ([s (sin t)]
        [c (cos t)])
    (matrix [[   c  s 0]
             [(- s) c 0]
             [   0  0 1]])))

(define (transform-point p m)
  (let ([t (matrix* m (array #[#[(posn-x p)]#[(posn-y p)]#[1]]))])
    (make-posn (array-ref t #(0 0)) (array-ref t #(1 0)))))

(define (transform-list ps m)
  (map (lambda (p) (transform-point p m)) ps))

(require 2htdp/image)

; generate random walk, always returning to where started
; random positive/negative pairs, then shuffled
(define (random-symetric-walk wobble stability len)
  (define (scan f i l)
    (if (empty? l) l (let ([ii (f i (first l))]) (cons ii (scan f ii (rest l))))))
  (define (random-pairs walk n)
    (if (zero? n) '()
        (let ([r (if (= 0 (random stability)) (random wobble) 0)])
          (cons r (cons (- r) (random-pairs walk (sub1 n)))))))
  (scan (lambda (a b) (+ a b)) 0
        (shuffle (random-pairs '() (/ len 2)))))
  
(define (make-asteroid-polygon num-points radius bumpyness jaggyness intermittence)
  (define (make-wiggly-radius wiggle)
    (map (lambda (b) (max 0 (+ b radius))) wiggle))
  (let* ([angles (map (lambda (s) (* 2 pi (/ s num-points)))
                      (sequence->list (in-range 0 num-points)))]
         [bumps (make-wiggly-radius (random-symetric-walk bumpyness 1 num-points))]
         [jags (make-wiggly-radius (random-symetric-walk jaggyness intermittence num-points))])
    (map (lambda (a b j) (let* ([r (+ b j)]
                                [x (* r (cos a))]
                                [y (* r (sin a))])
                         (make-posn x y)))
         angles bumps jags)))

(struct asteroid (polygon pos orient velocity angular))

(define WIDTH 512)
(define HEIGHT 300)

(define (make-random-asteroid)
  (define RADIUS (+ 5 (random 15)))
  (define NUM_POINTS 50)
  (define BUMPYNESS (inexact->exact (round (* RADIUS 0.2))))
  (define JAGGYNESS (inexact->exact (round (* RADIUS 1.5))))
  (define INTERMITTENCE (/ NUM_POINTS 5))
  (define MOTION 2)
  (define SPIN 4)
  (define SCALE 1)
  (asteroid
   (make-asteroid-polygon NUM_POINTS RADIUS BUMPYNESS JAGGYNESS INTERMITTENCE)
   (translation-matrix (+ (random (/ WIDTH 2)) (/ WIDTH 4))
                       (+ (random (/ HEIGHT 2)) (/ HEIGHT 4)))
   (rotation-matrix 0)
   (matrix* (translation-matrix (- (random MOTION) (/ MOTION 2))
                                (- (random MOTION) (/ MOTION 2)))
            (let ([s (- 1 (/ (- (random SCALE) (/ SCALE 2)) 100))])
              (scale-matrix s s)))
   (rotation-matrix (/ (- (random SPIN) (/ SPIN 2)) 10))))
  
(define initial-asteroids
  (map (lambda (x) (make-random-asteroid)) (sequence->list (in-range 0 10))))

(define (update asteroids)
  (map (lambda (a)
         (let* ([vel (asteroid-velocity a)]
                [ang (asteroid-angular a)]
                [pos (matrix* (asteroid-pos a) vel)]
                [orient (matrix* (asteroid-orient a) ang)])
           (asteroid (asteroid-polygon a) pos orient vel ang)))
       asteroids))

(define (render asteroids)
  (foldl (lambda (a scene) (scene+polygon scene a "outline" "white"))
         (empty-scene WIDTH HEIGHT "black")
         (map (lambda (a)
                (let ([poly (asteroid-polygon a)]
                      [pos (asteroid-pos a)]
                      [orient (asteroid-orient a)])
                  (transform-list poly (matrix* pos orient)))) asteroids)))

(require 2htdp/universe)

(big-bang initial-asteroids
          (on-tick update 1/30)
          (to-draw render)
          (record? true))

