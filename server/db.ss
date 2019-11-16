;; Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket
(require (planet jaymccarthy/sqlite:5:1/sqlite))
(provide (all-defined-out))
(require "logger.ss")

(define (ms->frac ms)
  (modulo (inexact->exact (round ms)) 1000))

(define (timestamp-now)
  (let* ((ms (current-inexact-milliseconds))
         (t (seconds->date (inexact->exact (round (/ ms 1000))))))
    (string-append
     (number->string (date-year t)) "-"
     (substring (number->string (+ (date-month t) 100)) 1 3) "-"
     (substring (number->string (+ (date-day t) 100)) 1 3) " "
     (substring (number->string (+ (date-hour t) 100)) 1 3) ":"
     (substring (number->string (+ (date-minute t) 100)) 1 3) ":"
     (substring (number->string (+ (date-second t) 100)) 1 3) "."
     ;; get fractional second from milliseconds
     (substring (number->string (+ (ms->frac ms) 1000)) 1 4)
     )))

(define (setup db)
  (exec/ignore db "create table player ( id integer primary key autoincrement)")
  (exec/ignore db "create table game ( id integer primary key autoincrement, player_id integer, location varchar, time_stamp varchar, new_nests integer, num_workers_laid integer, num_workers_hatched integer, cells_built integer, events_survived integer, num_reproductives_hatched integer, energy_foraged real, survival_time real)")
  (exec/ignore db "create table player_name ( id integer primary key autoincrement, player_id integer, player_name text )")
  )

(define (insert-player db)
  (insert db "INSERT INTO player VALUES (NULL)"))

(define (insert-game db player_id location)
  (insert
   db "INSERT INTO game VALUES (NULL, ?, ?, ?, 0, 0, 0, 0, 0, 0, 0, 0)"
   player_id location (timestamp-now)))

(define (update-score db game_id new_nests num_workers_laid num_workers_hatched cells_built events_survived num_reproductives_hatched energy_foraged survival_time)
  (exec/ignore
   db "update game set new_nests=?, num_workers_laid=?, num_workers_hatched=?, cells_built=?, events_survived=?, num_reproductives_hatched=?, energy_foraged=?, survival_time=? where id = ?"
   new_nests num_workers_laid num_workers_hatched cells_built events_survived
   num_reproductives_hatched energy_foraged survival_time
   game_id))

(define (insert-player-name db player_id player_name)
  (insert db "insert into player_name VALUES (NULL, ?, ?)"
          player_id player_name ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get a list of all the scores
(define (get-game-scores db)
  (let* ((s (select db "select g.new_nests from game as g
                        join player_name as n on g.player_id=n.player_id
                        where n.player_name != '??'
                        order by new_nests, survival_time")))
    (if (null? s)
        '()
        (map
         (lambda (i) (vector-ref i 0))
         (cdr s)))))

;; get the player name/scores ordered for the hiscores list
(define (hiscores-select db)
  (let ((r (select db "select n.player_name, g.new_nests, g.survival_time from game as g
                     join player_name as n on g.player_id=n.player_id        
                     where n.player_name !='???'
                     order by g.new_nests desc, g.survival_time desc limit 10")))
    (if (null? r) '() (cdr r))))

(define (get-position v ol)
  (define (_ n l)
    (cond
      ((null? l) n)
      ((> (car l) v) n)
      (else (_ (+ n 1) (cdr l)))))
  (_ 1 ol))

(define (get-game-rank db game-id)
  (let ((s (select db "select new_nests from game where id=?" game-id)))
    (if (null? s)
	999
	(get-position (vector-ref (cadr s) 0) (get-game-scores db)))))

