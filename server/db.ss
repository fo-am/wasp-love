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

(define (setup db)
  (exec/ignore db "create table player ( id integer primary key autoincrement)")
  (exec/ignore db "create table game ( id integer primary key autoincrement, player_id integer, time_stamp varchar, new_nests integer, num_wasps_hatched integer, cells_built integer, num_reproductives_hatched integer, energy_foraged real, survival_time real)")
  (exec/ignore db "create table player_name ( id integer primary key autoincrement, player_id integer, player_name text )")
  )

(define (insert-player db)
  (insert db "INSERT INTO player VALUES (NULL)"))

(define (insert-game db player_id new_nests num_wasps_hatched cells_built num_reproductives_hatched energy_foraged survival_time)
  (insert
   db "INSERT INTO game VALUES (NULL, ?, ?, ?, ?, ?, ?, ?)"
   player_id new_nests num_wasps_hatched cells_built
   num_reproductives_hatched energy_foraged survival_time))

(define (update-score db game_id new_nests num_wasps_hatched cells_built num_reproductives_hatched energy_foraged survival_time)
  (exec/ignore
   db "update game set new_nests=?, num_wasps_hatched=?, cells_built=?, num_reproctives_hatched=?, energy_foraged=?, survival_time=? where id = ?"
   new_nests num_wasps_hatched cells_built
   num_reproductives_hatched energy_foraged survival_time
   game_id))

(define (insert-player-name db player_id player_name)
  (insert db "insert into player_name VALUES (NULL, ?, ?)"
          player_id player_name ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get a list of all the scores
(define (get-game-scores db)
  (let* ((s (select db "select new_nests from game order by new_nests")))
    (if (null? s)
        '()
        (map
         (lambda (i) (vector-ref i 0))
         (cdr s)))))

;; get the player name/scores ordered for the hiscores list
(define (hiscores-select db)
  (let ((r (select db "select n.player_name, g.new_nests from game as g
                     join player_name as n on g.player_id=n.player_id                     
                     order by g.new_nests limit 100")))
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
    (display s)(newline)
    (if (null? s)
	999
	(get-position (cadr s) (get-game-scores db)))))

