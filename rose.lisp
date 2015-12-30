#!/usr/bin/env sbcl --noinform --core qlsblc --script

;;; Petals Around the Rose
;;;

(load "mattos/mattos.lisp")
(load "curses.lisp")


(setf *random-state* (make-random-state t))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defun win-reset (w)
  (curses:werase w)
  (curses:box w 0 0)
  (curses:wrefresh w))

(defun corners (tly tlx h w)
  (curses:mvaddspch tly tlx curses:*ACS_ULCORNER*)
  (curses:mvaddspch tly (+ tlx w) curses:*ACS_URCORNER*)
  (curses:mvaddspch (+ tly h) tlx curses:*ACS_LLCORNER*)
  (curses:mvaddspch (+ tly h) (+ tlx w) curses:*ACS_LRCORNER*))

(defun draw-rect (tly tlx h w)
  (curses:move tly tlx)
  (curses:hline 0 w)
  (curses:move tly tlx)
  (curses:vline 0 h)
  (curses:move (+ tly h) tlx)
  (curses:hline 0 w)
  (curses:move (1+ tly) (+ tlx w))
  (curses:vline 0 h)
  (corners tly tlx h w))

(defun symbol-row (symb starty startx n step)
  (do ((i 0 (+ i 1)))
    ((>= i n))
    (curses:mvaddspch starty (+ startx (* i step)) symb))
  (curses:refresh))

(mattos:defobject die (num height width top-left-x top-left-y symb))

(mattos:defmeth die edges ()
                (draw-rect top-left-y top-left-x height width))

(mattos:defmeth die one ()
                (let ((x (+ top-left-x (round (/ width 2))))
                      (y (+ top-left-y (round (/ height 2)))))
                  (curses:mvaddspch y x symb)
                  (curses:refresh)))


(mattos:defmeth die two ()
                (curses:mvaddspch (round (+ top-left-y (* 2 (/ height 3))))
                                  (round (+ top-left-x (/ width 3)))
                                  symb)
                (curses:mvaddspch (round (+ top-left-y (/ height 3)))
                                  (round (+ top-left-x (* 2 (/ width 3))))
                                  symb)
                (curses:refresh))

(mattos:defmeth die three ()
                (mattos:this 'one)
                (mattos:this 'two)
                (curses:refresh))

(mattos:defmeth die four ()
                (let ((starty (round (+ top-left-y (/ height 4))))
                      (startx (floor (+ top-left-x (/ width 3))))
                      (step   (floor (* 2 (/ width 5)))))
                  (symbol-row symb starty startx 2 step)
                  (symbol-row symb (round (+ starty (* 3 (/ height 5)))) startx 2 step)))

(mattos:defmeth die five ()
                (mattos:this 'one)
                (mattos:this 'four)
                (curses:refresh))

(mattos:defmeth die six ()
                (mattos:this 'four)
                (symbol-row symb
                            (round (+ top-left-y (/ height 2)))
                            (floor (+ top-left-x (/ width  3)))
                            2
                            (floor (* 2 (/ width 5))))
                (curses:refresh))

(mattos:defmeth die display (n)
                (mattos:this 'edges)
                (case n
                  (1 (mattos:this 'one))
                  (2 (mattos:this 'two))
                  (3 (mattos:this 'three))
                  (4 (mattos:this 'four))
                  (5 (mattos:this 'five))
                  (6 (mattos:this 'six))))

(mattos:defmeth die roll ()
                (let ((n (1+ (random 6 *random-state*))))
                  (setf num n)
                  (mattos:this 'display n)))

(defun create-win (h w starty startx)
  (curses:refresh)
  (let ((localwin (curses:newwin h w starty startx)))
    (curses:box localwin 0 0)
    (curses:wrefresh localwin)
    localwin))

(defparameter pw nil)

(defun prompt ()
  (win-reset pw)
  (curses:mvwprintw pw 1 1 "> ")
  (curses:wrefresh pw)
  (curses:refresh)
  (curses:mvwgetstr pw 1 3))

(die d1)
(d1 'height 10)
(d1 'width  20)
(d1 'top-left-x 10)
(d1 'top-left-y 10)
(d1 'symb curses:*ACS_DIAMOND*)

(die d2)
(d2 'height 10)
(d2 'width  20)
(d2 'top-left-x 45)
(d2 'top-left-y 10)
(d2 'symb curses:*ACS_DIAMOND*)

(die d3)
(d3 'height 10)
(d3 'width  20)
(d3 'top-left-x 80)
(d3 'top-left-y 10)
(d3 'symb curses:*ACS_DIAMOND*)

(die d4)
(d4 'height 10)
(d4 'width  20)
(d4 'top-left-x 115)
(d4 'top-left-y 10)
(d4 'symb curses:*ACS_DIAMOND*)

(die d5)
(d5 'height 10)
(d5 'width  20)
(d5 'top-left-x 150)
(d5 'top-left-y 10)
(d5 'symb curses:*ACS_DIAMOND*)

(defparameter *dice* (list #'d1 #'d2 #'d3 #'d4 #'d5))

(defun mapmethod (m objs)
  (mapcar #'(lambda (obj) (funcall obj m)) objs))

(defun petals (d)
  (case (funcall d 'num)
    (1 0)
    (2 0)
    (3 2)
    (4 0)
    (5 4)
    (6 0)))

(defun petals-around-the-rose (dice)
  (reduce #'+ (map 'list #'(lambda (d) (petals d)) dice)))

(defun win (winning-answer)
  (curses:mvprintw 60 5 (format nil "You guessed it! The right answer is ~A. Hit any key to leave" winning-answer))
  (curses:getch))

(defun play ()
  (mapmethod 'roll *dice*)
  (do ((answer (parse-integer (prompt))
               (parse-integer (prompt))))
    ((= answer (petals-around-the-rose *dice*)) (win answer))
    (curses:erase)
    (curses:mvprintw 60 5 (format nil "Sorry, ~A is the wrong answer. Try again" answer))
    (mapmethod 'roll *dice*)
    (curses:move 0 0)))


(curses:connect-console)
(setf pw (create-win 3 140 65 0))
(play)

(curses:close-console)
