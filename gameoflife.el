;;; gameoflife.el --- Famous John Conway’s game of life! -*- lexical-binding: t -*-

;; Author: Arthur Léothaud
;; Maintainer: Arthur Léothaud
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: game life conway


;; This file is not part of GNU Emacs

;; This project is libre, and licenced under the terms of the
;; DO WHAT THE FUCK YOU WANT TO PUBLIC LICENCE, version 3.1,
;; as published by dtf on July 2019. See the COPYING file or
;; https://ph.dtf.wtf/w/wtfpl/#version-3-1 for more details.

;;; Commentary:

;; This is an attempt at an EMACS LISP version of the famous John Conway’s Game
;; of Life. Press n to go to the next generation and watch as the cells evolve.

;;; Code:

(defface gol-face-dead '((t . ( :background "white"
                                :foreground "white"
                                :color "white"
                                ;; :width ultra-expanded
                                ;; :height 200
                                ;; :box ( :line-width (1 . 1) :color "grey")
                                )))
  "Face for the dead cells"   :group 'gol-faces)
(defface gol-face-live '((t . ( :background "black"
                                :foreground "black"
                                :color "black"
                                ;; :width ultra-expanded
                                ;; :height 200
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

(defvar gol-rows 35
  "The board height.")

(defvar gol-columns 35
  "The board width.")

(define-derived-mode gol-mode special-mode "game-of-life-mode"
  (define-key gol-mode-map (kbd "w")     '(lambda () "" (interactive) (gol-init)))
  (define-key gol-mode-map (kbd "r")     '(lambda () "" (interactive) (gol-populate-random)))
  (define-key gol-mode-map (kbd "n")     '(lambda () "" (interactive) (gol-move)))
  (define-key gol-mode-map (kbd "<SPC>") '(lambda () "" (interactive) (gol-move))))

;;;###autoload
(defun game-of-life () "Start the Game of Life!"
       (interactive)
       (switch-to-buffer "*game of life*")
       (gol-mode)
       (gol-init))

(defun gol-init ()
  "Initialize the game."
  (interactive)
  (setq gol-board (make-vector (* gol-rows gol-columns) nil))
  (setq gol-move-count 0)
  (gol-draw-board))

(defun gol-populate-random ()
  "Populate the board with dead cells."
  (dotimes (row gol-rows)
    (dotimes (col gol-columns)
      (gol-set-cell gol-board row col
                    (cond ((eq 0 (random 2)) nil)
                          ((eq 1 (random 2)) t)))))
  (gol-draw-board))

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
  "Set the value VAL in BOARD at (ROW, COL) if different from current value."
  (unless (cell-eq (gol-get-cell row col) val)
    (aset board (gol-get-index row col) val)))

(defun cell-eq (cell1 cell2)
  "Return t if CELL1 and CELL2 have the same value, nil otherwise."
  (or (and cell1 cell2)
      (and (not cell1) (not cell2))))

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
    (insert-button (if val "⬛" "⬜")
                   'action (lambda (button)
                             (gol-flip-cell button))
                   'face face
                   'row row
                   'col col
                   'follow-link t
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
                    " to go forward one generation!\n"
                    " Type "
                    (propertize " w " 'face 'gol-face-button-next 'pointer 'finger)
                    " to wipe the board.\n"
                    " Type "
                    (propertize " r " 'face 'gol-face-button-next 'pointer 'finger)
                    " to populate the board with random cells."))))

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
  ;; set values to real board
  (update-board)
  ;; redraw the board
  (gol-draw-board))

(defun update-board ()
  "Update real board with values from tmp board."
  (setq gol-board gol-board-tmp))

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

(defun gol-flip-cell (button)
  "Change the state of cell at (ROW, COL) and update face of BUTTON."
  (interactive)
  (let* ((inhibit-read-only t)
         (row (button-get button 'row))
         (col (button-get button 'col))
         (new-val (not (gol-get-cell row col)))
         (new-face (gol-get-face new-val))
         (button-pos (button-start button)))
    (gol-set-cell gol-board row col new-val)
    (goto-char button-pos)
    (delete-char 1)
    (insert-button (if new-val "⬛" "⬜")
                   'action (lambda (button)
                             (gol-flip-cell button))
                   'face new-face
                   'row row
                   'col col
                   'follow-link t
                   'mouse t
                   'mouse-face 'highlight)))


(provide 'gameoflife)
;;; gameoflife.el ends here
