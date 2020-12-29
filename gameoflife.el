;;; gameoflife.el --- Famous John Conway’s game of life! -*- lexical-binding: t -*-

;; Author: Arthur Léothaud
;; Maintainer: Arthur Léothaud
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: game life conway


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is an attempt at an EMACS LISP version of the famous John Conway’s Game
;; of Life. Press n to go to the next generation and watch as the cells evolve.

;;; Code:

(defface gol-face-dead '((t . ( :background "black"
                                :foreground "black"
                                :color "black"
                                ;; :width ultra-expanded
                                :height 200
                                ;; :box ( :line-width (1 . 1) :color "grey")
                                )))
  "Face for the dead cells"   :group 'gol-faces)
(defface gol-face-live '((t . ( :background "yellow"
                                :foreground "yellow"
                                :color "yellow"
                                ;; :width ultra-expanded
                                :height 200
                                ;; :box ( :line-width (1 . 1) :color "grey")
                                )))
  "Face for the living cells" :group 'gol-faces)

(defface gol-face-button-next '((t . (:background "black"  :foreground "white" :weight bold))) "Face for the keys allowed" :group 'gol-faces)

(defvar gol-board nil
  "The matrix containing the values representing the cells.")

(defvar gol-board-tmp nil
  "The backup of the board while calculating all new cell states.")

(defvar gol-move-count 0
  "The number of moves performed.")

(defvar gol-rows 20
  "The board height.")

(defvar gol-columns 20
  "The board width.")

(define-derived-mode gol-mode special-mode "game-of-life-mode"
  (define-key gol-mode-map (kbd "n") '(lambda () "" (interactive) (gol-move)))
  (define-key gol-mode-map (kbd "<SPC>") '(lambda () "" (interactive) (gol-move))))

;;;###autoload
(defun game-of-life () "Start the Game of Life!"
       (interactive)
       (switch-to-buffer "*game of life*")
       (gol-mode)
       (text-scale-adjust -1)
       (gol-init))

(defun gol-init ()
  "Initialize the game."
  (interactive)
  (setq gol-board (make-vector (* gol-rows gol-columns) nil))
  ;; (gol-populate-board)
  (setq gol-move-count 0)
  (gol-draw-board))

(defun gol-populate-board ()
  "Populate the board with dead cells."
  (dotimes (row gol-rows)
    (dotimes (col gol-columns)
      (gol-set-cell gol-board row col
                    (cond ((eq 0 (random 2)) nil)
                          ((eq 1 (random 2)) t))))))

(defun gol-get-index (row col)
  "Get the index in the board for (ROW, COL)."
  (+ (* row gol-columns) col))

(defun gol-get-cell (row col)
  "Get the value in (ROW, COL)."
  (when (and (>= row 0)
             (< row gol-rows)
             (>= col 0)
             (< col gol-columns))
    (elt gol-board (gol-get-index row col))))

(defun gol-set-cell (board row col val)
  "Set the value VAL in BOARD at (ROW, COL)."
  (aset board (gol-get-index row col) val))

(defun gol-get-face (value)
  "Get face for VALUE."
  (intern (concat "gol-face-"
                  (if value "live"
                    "dead"))))

(defun gol-draw-cell (row col)
  "Redraw cell (ROW, COL)."
  (let* ((inhibit-read-only t)
         (val (gol-get-cell row col))
         (face (gol-get-face val)))
    (insert-button "█"
                   'action (lambda (button)
                             (gol-click-on-cell row col button))
                   'face face
                   'mouse t
                   'mouse-face 'highlight)))

(defun gol-draw-board ()
  "Draw the board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n")

    ;; for each row
    (dotimes (row gol-rows)
      (insert " ")
      ;; print the cells of each line
      (dotimes (col gol-columns)
        (gol-draw-cell row col))
      (insert "\n"))
    (insert (concat "\n Generation: " (number-to-string gol-move-count) "\n\n"
                    " Type "
                    (propertize " n " 'face 'gol-face-button-next 'pointer 'finger)
                    " or "
                    (propertize " SPACE " 'face 'gol-face-button-next 'pointer 'finger)
                    " to go forward one generation!"))))

(defun gol-move ()
  "Play a move, applying the rules on every cell, one time."
  ;; make a copy of the board while calculating all new cell states
  (setq gol-board-tmp (copy-tree gol-board t))
  ;; for each row...
  (dotimes (row gol-rows)
    ;; for each column...
    (dotimes (col gol-columns)
      ;; calculate the new state (dead/alive) for that cell
      ;; and put it back in the board
      (gol-set-cell gol-board-tmp row col (compute-new-cell-state row col))))
  ;; increment move count
  (setq-local gol-move-count (1+ gol-move-count))
  ;; redraw the board
  (setq gol-board gol-board-tmp)
  (gol-draw-board))

(defun compute-new-cell-state (row col)
  "Calculate the new state at (ROW, COL)."
  (let* ((former-state (gol-get-cell row col))
         (live-neighbours (gol-count-live-neighbours row col)))
    (cond
     ((> live-neighbours 3) nil) ;; overpopulation
     ((< live-neighbours 2) nil) ;; loneliness
     ((= live-neighbours 2) former-state) ;; do not change state
     ((= live-neighbours 3) t)))) ;; alive

(defun gol-count-live-neighbours (row col)
  "Count the number of live cells surrounding (ROW, COL)."
  (+ (if (gol-get-cell (1- row) (1- col)) 1 0)
     (if (gol-get-cell (1- row)     col ) 1 0)
     (if (gol-get-cell (1- row) (1+ col)) 1 0)
     (if (gol-get-cell     row  (1- col)) 1 0)
     (if (gol-get-cell     row  (1+ col)) 1 0)
     (if (gol-get-cell (1+ row) (1- col)) 1 0)
     (if (gol-get-cell (1+ row)     col ) 1 0)
     (if (gol-get-cell (1+ row) (1+ col)) 1 0)))

(defun gol-click-on-cell (row col button)
  "Change the state of cell at (ROW, COL) and update face of BUTTON."
  (interactive)
  (let* ((new-val (not (gol-get-cell row col)))
         (new-face (gol-get-face new-val)))
    (gol-set-cell gol-board row col new-val)
    (button-put button 'face new-face)))

(provide 'gameoflife)
;;; gameoflife.el ends here
