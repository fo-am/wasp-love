#!/usr//bin/env mzscheme
#lang scheme/base
;; Naked on Pluto Copyright (C) 2010 Aymeric Mansoux, Marloes de Valk, Dave Griffiths
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

(require scheme/system
         scheme/foreign
         scheme/cmdline
         web-server/servlet
         web-server/servlet-env
         web-server/http/response-structs
         "server/request.ss"
         "server/logger.ss"
         "server/json.ss"
         "server/db.ss")

(require (planet jaymccarthy/sqlite:5:1/sqlite))

; a utility to change the process owner,
; assuming mzscheme is called by root.
;;(unsafe!)
;;(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))

(define db-name "wasps.db")
(define db #f)

(if (file-exists? (string->path db-name))
    (begin
      (display "open existing db")(newline)
      (set! db (open (string->path db-name))))
    (begin
      (display "makine new db")(newline)
      (set! db (open (string->path db-name)))
      (setup db)))

(open-log "log.txt")

(define (pluto-response txt)
  (response/full
   200                ; code
   #"Okay"            ; message
   (current-seconds)  ; seconds
   #"text/javascript" ; mime type
   '()                ; headers
   (list (string->bytes/utf-8 txt)))) ; body

(define registered-requests
  (list
   (register
    (req 'ping '())
    (lambda ()
      (pluto-response (scheme->json '("hello")))))

   (register
    (req 'player '(played_before age_range))
    (lambda (played-before age-range)
      (let* ((id (insert-player db played-before age-range)))
        (pluto-response (scheme->json (list id))))))

   (register
    (req 'game '(player_id species))
    (lambda (player_id species)
      (let* ((id (insert-game db player_id species)))
        (pluto-response (scheme->json (list id))))))

   (register
    (req 'click '(game_id
                  photo_name
                  crab_name
                  photo_habitat
                  crab_habitat
                  crab_x
                  crab_y
                  crab_rot
                  time_stamp
                  x_position
                  y_position
                  success))
    (lambda (game_id
             photo_name
             crab_name
             photo_habitat
             crab_habitat
             crab_x
             crab_y
             crab_rot
             time_stamp
             x_position
             y_position
             success)
      (let* ((id (insert-click
                  db
                  game_id
                  photo_name
                  crab_name
                  photo_habitat
                  crab_habitat
                  (number->string (inexact->exact (round (string->number crab_x))))
                  (number->string (inexact->exact (round (string->number crab_y))))
                  crab_rot
                  time_stamp
                  x_position
                  y_position
                  success)))
        (pluto-response (scheme->json '())))))

   (register
    (req 'crab-time '(game_id
                      photo_name
                      crab_name
                      photo_habitat
                      crab_habitat
                      time_stamp
                      success_code))
    (lambda (game_id
             photo_name
             crab_name
             photo_habitat
             crab_habitat
             time_stamp
             success_code)
      (let* ((id (insert-crab-time
                  db
                  game_id
                  photo_name
                  crab_name
                  photo_habitat
                  crab_habitat
                  time_stamp
                  success_code
                  )))
        (pluto-response (scheme->json '())))))

   (register
    (req 'score '(game_id))
    (lambda (game_id)
      (let ((av (get-game-average db (string->number game_id)))
            (c (get-game-count db (string->number game_id))))
        (pluto-response
         (scheme->json (list av
                             (get-game-rank db game_id av)
                             (if (not c) 0 c)))))))

   (register
    (req 'hiscores '())
    (lambda ()
      (pluto-response
       (scheme->json (get-hiscores db)))))

   (register
    (req 'stats '())
    (lambda ()
      (pluto-response
       (scheme->json (get-stats db)))))

   (register
    (req 'player-name '(player_id player_name))
    (lambda (player_id player_name)
      (insert-player-name db player_id player_name)
      (pluto-response (scheme->json '()))))))

(define (start request)
  (let ((values (url-query (request-uri request))))
    ;(msg values)
    (if (not (null? values))   ; do we have some parameters?
        (let ((name (assq 'fn values)))
          (if name           ; is this a well formed request?
              (request-dispatch
               registered-requests
               (req (string->symbol (cdr name))
                    (filter
                     (lambda (v)
                       (not (eq? (car v) 'fn)))
                     values)))
              (pluto-response "bad formed request thing")))
        (pluto-response "malformed thingy"))))

(printf "server is running...~n")

; Here we become the user 'nobody'.
; This is a security rule that *only works* if nobody owns no other processes
; than mzscheme. Otherwise better create another dedicated unprivileged user.
; Note: 'nobody' must own the state directory and its files.

;(setuid 65534)

;;

(serve/servlet
 start
 ;; port number is read from command line as argument
 ;; ie: ./server.scm 8080
 #:port (string->number (command-line #:args (port) port))
 #:listen-ip "127.0.0.1"
 #:command-line? #t
 #:servlet-path "/game"
 #:server-root-path
 (build-path "client"))
