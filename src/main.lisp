(in-package :blastok)

;; --- General ---

(defvar *FRAMES* 0)
(defvar *SCORE* 0)

(defgeneric must-init (test desc))

(defmethod must-init ((test sb-sys::system-area-pointer) desc)
  (when (null-pointer-p test)
    (if (>  (al:get-errno) 0)
        (error "Couldn't initialize ~s (~d)." desc (al:get-errno))
        (error "Couldn't initialize ~s." desc))))

(defmethod must-init ((test t) description)
  (unless test
    (error "Couldn't initialize ~s." description)))

(defun between (lo hi)
  (let* ((r (+ lo (* (random 1.0) (- hi lo))))
		 (result (truncate r)))
	result))

(defun between-f (lo hi)
  (+ lo (* (random 1.0) (- hi lo))))

(defun collide (left1 top1 right1 bottom1 left2 top2 right2 bottom2)
  (cond ((> left1 right2) (return-from collide nil))
		((< right1 left2) (return-from collide nil))
		((> top1 bottom2) (return-from collide nil))
		((< bottom1 top2) (return-from collide nil)))
  t)

;; --- Display ---

(defconstant +BUFFER-W+ 320 "Draw buffer width in pixels.")
(defconstant +BUFFER-H+ 240 "Draw buffer height in pixels.")
(defconstant +DISP-SCALE+ 3 "Scale to draw to screen from draw buffer.")
(defconstant +DISP-W+ (* +BUFFER-W+ +DISP-SCALE+) "Total width in pixels.")
(defconstant +DISP-H+ (* +BUFFER-H+ +DISP-SCALE+) "Total height in pixels.")
(defvar *DISP* (null-pointer) "Main screen display.")
(defvar *BUFFER* (null-pointer) "Draw buffer.")

(defun disp-init ()
  "Initialize display."
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 8 :suggest)
  ;; Allocate screen
  (setf *DISP* (al:create-display +DISP-W+ +DISP-H+))
  (must-init *DISP* "display")
  ;; Allocate draw buffer
  (setf *BUFFER* (al:create-bitmap +BUFFER-W+ +BUFFER-H+))
  (must-init *BUFFER* "bitmap buffer"))

(defun disp-deinit ()
  "Clean up display."
  (al:destroy-bitmap *BUFFER*)
  (setf *BUFFER* (null-pointer))
  (al:destroy-display *DISP*)
  (setf *DISP* (null-pointer)))

(defun disp-pre-draw ()
  "Prepare to draw game user interface."
  (al:set-target-bitmap *BUFFER*))

(defun disp-post-draw ()
  "Finish drawing game user interface."
  (al:set-target-backbuffer *DISP*)
  (al:draw-scaled-bitmap *BUFFER* 0 0 +BUFFER-W+ +BUFFER-H+ 0 0 +DISP-W+ +DISP-H+ 0)
  (al:flip-display))

;; --- Keyboard ---

(defun key-code (v)
  "Return allegro key code for specified key.  The value can be used as index into
   the allegro keys array, or our own *KEY* array."
  (foreign-enum-value 'al::keycodes v))

(defconstant +KEY-MAX+ (foreign-enum-value 'al::keycodes :key-max)
  "Allegro's maximum key code.  Used to size our *KEY* array.")

(defconstant +KEY-DOWN+ (foreign-enum-value 'al::keycodes :down) "Down arrow key code.")
(defconstant +KEY-LEFT+ (foreign-enum-value 'al::keycodes :left) "Left arrow key code.")
(defconstant +KEY-RIGHT+ (foreign-enum-value 'al::keycodes :right) "Right arrow key code.")
(defconstant +KEY-UP+ (foreign-enum-value 'al::keycodes  :up) "Up arrow key code.")
(defconstant +KEY-X+ (foreign-enum-value 'al::keycodes :x) "x key code")
(defconstant +KEY-ESC+ (foreign-enum-value 'al::keycodes  :escape) "Escape key code.")
(defconstant +KEY-B+ (foreign-enum-value 'al::keycodes :b) "b key code")

(defvar *KEY-STATE-SEEN* (byte 1 0)
  "Within our *key* array, the false (0) and true (1) bit for any specified key
   having been seen before (regardless of whether its currently down or up).")
(defvar *KEY-STATE-DOWN* (byte 1 1)
  "Within out *key* array, the false (0) and true (1) bit for any specified key
   currently being down.")

(defvar *KEY* (make-array +KEY-MAX+ :adjustable nil)
  "State of keys.  Used instead of allegro's version because we need to know
   when a key has been seen before, not just whether its currently down.")

(defun keyboard-init ()
  "Initialize keyboard."
  (dotimes (index +KEY-MAX+)
	(setf (aref *KEY* index) 0)))

(defun keyboard-deinit ()
  "Claen up keyboard"
  nil)

(defun keyboard-update (event)
  "Handle keyboard event related logic."
  (case (foreign-slot-value event '(:union al:event) 'al::type)
	(:timer (dotimes (index +KEY-MAX+)
			  (let ((old (aref *KEY* index)))
				(setf (aref *KEY* index) (dpb 0 *KEY-STATE-SEEN* old)))))
	(:key-down (let* ((key (foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode))
					  (keyval (foreign-enum-value 'al::keycodes key)))
 				 (setf (aref *KEY* keyval) (dpb 1 *KEY-STATE-DOWN* (aref *KEY* keyval)))
 				 (setf (aref *KEY* keyval) (dpb 1 *KEY-STATE-SEEN* (aref *KEY* keyval)))))
	(:key-up (let* ((key (foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode))
					(keyval (foreign-enum-value 'al::keycodes key))
 					(old (aref *KEY* keyval)))
 			   (setf (aref *KEY* keyval) (dpb 0 *KEY-STATE-DOWN* old))))))

;; --- Sprites ---

(defconstant +SHIP-W+ 12 "Width of ship bitmap in pixels.")
(defconstant +SHIP-H+ 13 "Height of ship bitmap in pixels.")

(defconstant +SHIP-SHOT-W+ 2 "Width of ship shot in pixels.")
(defconstant +SHIP-SHOT-H+ 9 "Height of ship shot in pixels.")

(defconstant +LIFE-W+ 6)
(defconstant +LIFE-H+ 6)

(defvar *ALIEN-W* (make-array 3 :initial-contents #(14 13 45) :adjustable nil))
(defvar *ALIEN-H* (make-array 3 :initial-contents #(9 10 27) :adjustable nil))

(defvar +ALIEN-BUG-W+ (aref *ALIEN-W* 0))
(defvar +ALIEN-BUG-H+ (aref *ALIEN-H* 0))
(defvar +ALIEN-ARROW-W+ (aref *ALIEN-W* 1))
(defvar +ALIEN-ARROW-H+ (aref *ALIEN-H* 1))
(defvar +ALIEN-THICCBOI-W+ (aref *ALIEN-W* 2))
(defvar +ALIEN-THICCBOI-H+ (aref *ALIEN-H* 2))

(defconstant +ALIEN-SHOT-W+ 4)
(defconstant +ALIEN-SHOT-H+ 4)

(defconstant +EXPLOSION-FRAMES+ 4)
(defconstant +SPARKS-FRAMES+ 3)

(defstruct SPRITES
  (sheet (null-pointer))
  (ship (null-pointer))
  (ship-shot (make-array 2 :initial-element (null-pointer) :adjustable nil))
  (life (null-pointer))
  (alien (make-array 3 :initial-element (null-pointer) :adjustable nil))
  (alien-shot (null-pointer))
  (explosion (make-array +EXPLOSION-FRAMES+ :initial-element (null-pointer) :adjustable nil))
  (sparks (make-array +SPARKS-FRAMES+ :initial-element (null-pointer) :adjustable nil))
  (powerup (make-array 4 :initial-element (null-pointer) :adjustable nil)))
(defvar *SPRITES* (make-sprites))

(defun sprite-grab(x y w h)
  (let ((sprite (al:create-sub-bitmap (sprites-sheet *SPRITES*) x y w h)))
	(must-init sprite "sprite grab")
	sprite))

(defmacro sprites-ship-shots (o i)
  "Syntactic sugar."
    `(aref (sprites-ship-shot ,o) ,i))

(defmacro sprites-aliens (o i)
  "Syntactic sugar."
  `(aref (sprites-alien ,o) ,i))

(defmacro sprites-explosions (o i)
  "Syntactic sugar."
  `(aref (sprites-explosion ,o) ,i))

(defmacro sprites-sparkses (o i)
  "Syntactic sugar."
  `(aref (sprites-sparks ,o) ,i))

(defmacro sprites-powerups (o i)
  "Syntactic sugar."
  `(aref (sprites-powerup ,o) ,i))

(defun sprites-init ()
  (setf (sprites-sheet *SPRITES*) (al:load-bitmap (asset "spritesheet.png")))
  (must-init (sprites-sheet *SPRITES*) "spritesheet")
  (setf (sprites-ship *SPRITES*) (sprite-grab 0 0 +SHIP-W+ +SHIP-H+))
  (setf (sprites-ship-shots *SPRITES* 0) (sprite-grab 13 0 +SHIP-SHOT-W+ +SHIP-SHOT-H+))
  (setf (sprites-ship-shots *SPRITES* 1) (sprite-grab 16 0 +SHIP-SHOT-W+ +SHIP-SHOT-H+))
  (setf (sprites-life *SPRITES*) (sprite-grab 0 14 +LIFE-W+ +LIFE-H+))
  (setf (sprites-aliens *SPRITES* 0) (sprite-grab 19 0 +ALIEN-BUG-W+ +ALIEN-BUG-H+))
  (setf (sprites-aliens *SPRITES* 1) (sprite-grab 19 10 +ALIEN-ARROW-W+ +ALIEN-ARROW-H+))
  (setf (sprites-aliens *SPRITES* 2) (sprite-grab 0 21 +ALIEN-THICCBOI-W+ +ALIEN-THICCBOI-H+))
  (setf (sprites-alien-shot *SPRITES*) (sprite-grab 13 10 +ALIEN-SHOT-W+ +ALIEN-SHOT-H+))
  (setf (sprites-explosions *SPRITES* 0) (sprite-grab 33 10 9 9))
  (setf (sprites-explosions *SPRITES* 1) (sprite-grab 43 9 11 11))
  (setf (sprites-explosions *SPRITES* 2) (sprite-grab 46 21 17 18))
  (setf (sprites-explosions *SPRITES* 3) (sprite-grab 46 40 17 17))
  (setf (sprites-sparkses *SPRITES* 0) (sprite-grab 34 0 10 8))
  (setf (sprites-sparkses *SPRITES* 1) (sprite-grab 45 0 7 8))
  (setf (sprites-sparkses *SPRITES* 2) (sprite-grab 54 0 9 8))
  (setf (sprites-powerups *SPRITES* 0) (sprite-grab 0 49 9 12))
  (setf (sprites-powerups *SPRITES* 1) (sprite-grab 10 49 9 12))
  (setf (sprites-powerups *SPRITES* 2) (sprite-grab 20 49 9 12))
  (setf (sprites-powerups *SPRITES* 3) (sprite-grab 30 49 9 12)))

(defun sprites-deinit ()
  (al:destroy-bitmap (sprites-ship *SPRITES*))
  (al:destroy-bitmap (sprites-ship-shots *SPRITES* 0))
  (al:destroy-bitmap (sprites-ship-shots *SPRITES* 1))
  (al:destroy-bitmap (sprites-life *SPRITES*))
  (al:destroy-bitmap (sprites-aliens *SPRITES* 0))
  (al:destroy-bitmap (sprites-aliens *SPRITES* 1))
  (al:destroy-bitmap (sprites-aliens *SPRITES* 2))
  (al:destroy-bitmap (sprites-alien-shot *SPRITES*))
  (al:destroy-bitmap (sprites-explosions *SPRITES* 0))
  (al:destroy-bitmap (sprites-explosions *SPRITES* 1))
  (al:destroy-bitmap (sprites-explosions *SPRITES* 2))
  (al:destroy-bitmap (sprites-explosions *SPRITES* 3))
  (al:destroy-bitmap (sprites-sparkses *SPRITES* 0))
  (al:destroy-bitmap (sprites-sparkses *SPRITES* 1))
  (al:destroy-bitmap (sprites-sparkses *SPRITES* 2))
  (al:destroy-bitmap (sprites-powerups *SPRITES* 0))
  (al:destroy-bitmap (sprites-powerups *SPRITES* 1))
  (al:destroy-bitmap (sprites-powerups *SPRITES* 2))
  (al:destroy-bitmap (sprites-powerups *SPRITES* 3))
  (al:destroy-bitmap (sprites-sheet *SPRITES*)))

;; --- Audio ---

(defvar *SAMPLE-SHOT* (null-pointer))
(defvar *SAMPLE-EXPLODE* (make-array 2 :initial-element (null-pointer) :adjustable nil))

(defun asset (path)
  (namestring (asdf:system-relative-pathname "blastok" (concatenate 'string "assets/" path))))

(defun audio-init ()
  (al:install-audio)
  (al:init-acodec-addon)
  (al:reserve-samples 128)
  (setf *SAMPLE-SHOT* (al:load-sample (asset "shot.flac")))
  (must-init *SAMPLE-SHOT* "shot sample")
  (setf (aref *SAMPLE-EXPLODE* 0) (al:load-sample (asset "explode1.flac")))
  (must-init (aref *SAMPLE-EXPLODE* 0) "explode[0] sample")
  (setf (aref *SAMPLE-EXPLODE* 1) (al:load-sample (asset "explode2.flac")))
  (must-init (aref *SAMPLE-EXPLODE* 1) "explode[1] sample"))

(defun audio-deinit ()
  (al:destroy-sample *SAMPLE-SHOT*)
  (al:destroy-sample (aref *SAMPLE-EXPLODE* 0))
  (al:destroy-sample (aref *SAMPLE-EXPLODE* 1)))

;; --- FX ---

(defstruct FX
  (x 0 :type integer)
  (y 0 :type integer)
  (frame 0 :type integer)
  (spark nil :type boolean)
  (used nil :type boolean))

(defconstant +FX-N+ 128)
(defvar *FX* (make-array +FX-N+))

(defun fx-init ()
  (dotimes (index +FX-N+)
    (setf (aref *FX* index) (make-fx :x 0 :y 0 :frame 0 :spark nil :used nil))))

(defun fx-add (spark x y)
  (unless spark
	(let* ((once (foreign-enum-value 'al::playmode :once))
           (rand-sample (between 0 2))
           (sample (aref *SAMPLE-EXPLODE* rand-sample)))
     (al:play-sample sample 0.75 0 1 once (null-pointer))))
  (dotimes (index +FX-N+)
	(unless (fx-used (aref *FX* index))
	  (setf (fx-x (aref *FX* index)) x)
	  (setf (fx-y (aref *FX* index)) y)
	  (setf (fx-frame (aref *FX* index)) 0)
	  (setf (fx-spark (aref *FX* index)) spark)
	  (setf (fx-used (aref *FX* index)) t)
	  (return))))

(defun fx-update ()
  ;; For all fx's
  (dotimes (index +FX-N+)
	;; If its in use
	(when (fx-used (aref *FX* index))
	  ;; Incremenet next frame to show
	  (incf (fx-frame (aref *FX* index)) 1)
	  ;; If previous frame was last
	  (when (or (and (not (fx-spark (aref *FX* index)))
					 (eql (fx-frame (aref *FX* index)) (* +EXPLOSION-FRAMES+ 2)))
				(and (fx-spark (aref *FX* index))
					 (eql (fx-frame (aref *FX* index)) (* +SPARKS-FRAMES+ 2))))
		;; Disable the fx
		(setf (fx-used (aref *FX* index)) nil)))))

(defun fx-draw ()
  ;; For all fx's
  (dotimes (index +FX-N+)
	;; If its in use
	(when (fx-used (aref *FX* index))
	  ;; Draw the next frame
	  (let* ((frame-display (truncate (/ (fx-frame (aref *FX* index)) 2)))
			 (bmp (if (fx-spark (aref *FX* index))
					  (aref (sprites-sparks *SPRITES*) frame-display)
					  (aref (sprites-explosion *SPRITES*) frame-display)))
			 (x (- (fx-x (aref *FX* index)) (/ (al:get-bitmap-width bmp) 2)))
			 (y (- (fx-y (aref *FX* index)) (/ (al:get-bitmap-height bmp) 2))))
		(al:draw-bitmap bmp x y 0)))))

;; --- Shots ---

(defstruct SHOT
  (x 0 :type integer)
  (y 0 :type integer)
  (dx 0 :type integer)
  (dy 0 :type integer)
  (frame 0 :type integer)
  (ship nil :type boolean)
  (used nil :type boolean))
(defconstant +SHOTS-N+ 128)
(defvar *SHOTS* (make-array +SHOTS-N+ :adjustable nil))

(defun shots-init ()
  (dotimes (index +SHOTS-N+)
    (setf (aref *SHOTS* index) (make-shot :x 0 :y 0 :dx 0 :dy 0 :frame 0 :ship nil :used nil))))

(defun shots-add (ship straight x y)
  ;; User needs feedback regardless of whether we have a free shot
  (al:play-sample *SAMPLE-SHOT* 0.3 0
    			  (if ship 1.0 (between-f 1.5 1.6)) :once (null-pointer))

  (symbol-macrolet ((shot (aref *SHOTS* i)))
    (dotimes (i +SHOTS-N+)
      (unless (shot-used shot)
        (setf (shot-ship shot) ship)
        (if ship
            ;; Player ship
            (progn
              (setf (shot-x shot) (round (- x (/ +SHIP-SHOT-W+ 2))))
              (setf (shot-y shot) y))
            ;; Alien ship
            (progn
              (setf (shot-x shot) (round (- x (/ +ALIEN-SHOT-W+ 2)))
                    (shot-y shot) (round (- y (/ +ALIEN-SHOT-H+ 2))))
              (if straight
                  (setf (shot-dx shot) 0
                        (shot-dy shot) 2)
                  (setf (shot-dx shot) (between -2 2)
                        (shot-dy shot) (between -2 2)))
              (if (and (= (shot-dx shot) 0)
                       (= (shot-dy shot) 0))
                  (return-from shots-add t))
              (setf (shot-frame shot) 0)))
        (setf (shot-frame shot) 0)
        (setf (shot-used shot) t)
        (return-from shots-add t))))
  nil)

(defun shots-update ()
  (symbol-macrolet ((shot (aref *SHOTS* i)))
    (dotimes (i +SHOTS-N+)
      (block shots-loop
        (if (not (shot-used shot))
            (return-from shots-loop))
        (if (shot-ship shot)
            ;; Player ship shot
            (progn
              (decf (shot-y shot) 1)
              (if (< (shot-y shot) (- +SHIP-SHOT-H+))
                  (progn
                    (setf (shot-used shot) nil)
                    (return-from shots-loop))))
            ;; Alien ship shot
            (progn
              (incf (shot-x shot) (shot-dx shot))
              (incf (shot-y shot) (shot-dy shot))
              (if (or (< (shot-x shot) (- +ALIEN-SHOT-W+))
                      (> (shot-x shot) +BUFFER-W+)
                      (< (shot-y shot) (- +ALIEN-SHOT-H+))
                      (> (shot-y shot) +BUFFER-H+))
                  (progn
                    (setf (shot-used shot) nil)
                    (return-from shots-loop)))))
        (decf (shot-frame shot) 1)))))

(defun shots-collide (ship x y w h)
  (symbol-macrolet ((shot (aref *SHOTS* i)))
    (dotimes (i +SHOTS-N+)
      (block shots-loop
        (if (not (shot-used shot))
            (return-from shots-loop))
        (if (or (and ship (shot-ship shot))
                (and (not ship) (not (shot-ship shot))))
            (return-from shots-loop))
        (let ((sw)
              (sh))
          (if ship
            (setf sw +ALIEN-SHOT-W+
                  sh +ALIEN-SHOT-H+)
            (setf sw +SHIP-SHOT-W+
                  sh +SHIP-SHOT-H+))
          (if (collide x y (+ x w) (+ y h) (shot-x shot) (shot-y shot)
                       (+ (shot-x shot) sw) (+ (shot-y shot) sh))
              (progn
                (fx-add t (+ (shot-x shot) (round (/ sw 2))) (+ (shot-y shot) (round (/ sh 2))))
                (setf (shot-used shot) nil)
                (return-from shots-collide t)))))))
  nil)

(defun shots-draw ()
  (symbol-macrolet ((shot (aref *SHOTS* index)))
   (dotimes (index +SHOTS-N+)
	 (when (shot-used shot)
	   (let ((frame-display (truncate (mod (/ (shot-frame shot) 2) 2))))
		 (if (shot-ship shot)
			 (al:draw-bitmap (aref (sprites-ship-shot *SPRITES*) frame-display)
                             (shot-x shot) (shot-y shot) 0)
			 (let ((tint (if (> frame-display 0)
                             (al:map-rgb-f 1 1 1)
                             (al:map-rgb-f 0.5 0.5 0.5))))
			   (al:draw-tinted-bitmap (sprites-alien-shot *SPRITES*) tint
									  (shot-x shot) (shot-y shot) 0))))))))

;; --- Ships ---

(defconstant +SHIP-SPEED+ 3)
(defconstant +SHIP-MAX-X+ (- +BUFFER-W+ +SHIP-W+))
(defconstant +SHIP-MAX-Y+ (- +BUFFER-H+ +SHIP-H+))

(defstruct SHIP
  (x 0 :type integer)
  (y 0 :type integer)
  (shot-timer 0 :type integer)
  (lives 0 :type integer)
  (respawn-timer 0 :type integer)
  (invincible-timer 0 :type integer))
(defvar *SHIP* (make-ship))

(defun ship-init ()
  (setf (ship-x *SHIP*) (round (- (/ +BUFFER-W+ 2) (/ +SHIP-W+ 2))))
  (setf (ship-y *SHIP*) (round (- (/ +BUFFER-H+ 2) (/ +SHIP-H+ 2))))
  (setf (ship-shot-timer *SHIP*) 0)
  (setf (ship-lives *SHIP*) 3)
  (setf (ship-respawn-timer *SHIP*) 0)
  (setf (ship-invincible-timer *SHIP*) 120))

(defun ship-update ()
  ;; If no lives left, nothing to check
  (if (< (ship-lives *SHIP*) 0)
      (return-from ship-update))
  ;; If ship isn't active, nothing to do
  (if (> (ship-respawn-timer *SHIP*) 0)
      (progn
        (decf (ship-respawn-timer *SHIP*) 1)
        (return-from ship-update)))
  ;; Move horizontally if requested
  (if (> (aref *KEY* +KEY-LEFT+) 0)
      (decf (ship-x *SHIP*) +SHIP-SPEED+))
  (if (> (aref *KEY* +KEY-RIGHT+) 0)
      (incf (ship-x *SHIP*) +SHIP-SPEED+))
  ;; Move vertically if requested
  (if (> (aref *KEY* +KEY-UP+) 0)
      (decf (ship-y *SHIP*) +SHIP-SPEED+))
  (if (> (aref *KEY* +KEY-DOWN+) 0)
      (incf (ship-y *SHIP*) +SHIP-SPEED+))
  ;; Make sure ship stays in the screen area
  (if (< (ship-x *SHIP*) 0)
      (setf (ship-x *SHIP*) 0))
  (if (< (ship-y *SHIP*) 0)
      (setf (ship-y *SHIP*) 0))
  (if (> (ship-x *SHIP*) +SHIP-MAX-X+)
      (setf (ship-x *SHIP*) +SHIP-MAX-X+))
  (if (> (ship-y *SHIP*) +SHIP-MAX-Y+)
      (setf (ship-y *SHIP*) +SHIP-MAX-Y+))
  (if (> (ship-invincible-timer *SHIP*) 0)
      (decf (ship-invincible-timer *SHIP*) 1)
      ;; If a shot hits the ship
      (if (shots-collide t (ship-x *SHIP*) (ship-y *SHIP*) +SHIP-W+ +SHIP-H+)
          (progn
            (let ((x (round (+ (ship-x *SHIP*) (/ +SHIP-W+ 2))))
                  (y (round (+ (ship-y *SHIP*) (/ +SHIP-H+ 2)))))
              (fx-add nil x y)
              (fx-add nil (+ x 4) (+ y 2))
              (fx-add nil (- x 2) (- y 4))
              (fx-add nil (+ x 1) (- y 5))
              (decf (ship-lives *SHIP*) 1)
              (setf (ship-respawn-timer *SHIP*) 90)
              (setf (ship-invincible-timer *SHIP*) 180)))))
  ;; Can ship shoot?
  (if (> (ship-shot-timer *SHIP*) 0)
      ;; Nope, check next frame
      (decf (ship-shot-timer *SHIP*) 1)
      ;; If shoot requested ...
      (if (> (aref *KEY* +KEY-X+) 0)
          (progn
            (let ((x (round (+ (ship-x *SHIP*) (/ +SHIP-W+ 2)))))
              (if (shots-add t nil x (ship-y *SHIP*))
                  (progn
                    (setf (ship-shot-timer *SHIP*) 5)
                    )))))))

(defun ship-draw ()
  (when (and (> (ship-lives *SHIP*) 0)
             (= (ship-respawn-timer *SHIP*) 0))
    ;; If ship is invincible, only allow it to be drawn every 3rd frame so it blinks
    (if (= (mod (/ (ship-invincible-timer *SHIP*) 2) 3) 1)
        (return-from ship-draw))
    (al:draw-bitmap (sprites-ship *SPRITES*) (ship-x *SHIP*) (ship-y *SHIP*) 0)))

;; --- Aliens ---

(defconstant +ALIEN-TYPE-BUG+ 0)
(defconstant +ALIEN-TYPE-ARROW+ 1)
(defconstant +ALIEN-TYPE-THICCBOI+ 2)
(defconstant +ALIEN-TYPE-N+ 3)

(defstruct ALIEN
  (x 0 :type integer)
  (y 0 :type integer)
  (type 0 :type integer)
  (shot-timer 0 :type integer)
  (blink 0 :type integer)
  (life 0 :type integer)
  (used nil :type boolean))

(defconstant +ALIENS-N+ 16)
(defvar *ALIENS* (make-array +ALIENS-N+ :adjustable nil))

(defun aliens-init ()
  (dotimes (index +ALIENS-N+)
    (setf (aref *ALIENS* index) (make-alien :x 0 :y 0 :type 0 :shot-timer 0 :blink 0 :life 0 :used nil))))

(defun aliens-update ()
  (let ((new-quota (if (= 0 (mod *FRAMES* 120)) (between 2 4) 0))
		(new-x (between 10 (- +BUFFER-W+ 50))))
    ;; For each alien
    (dotimes (i (length *ALIENS*))
      (symbol-macrolet ((alien (aref *ALIENS* i)))
        (block each-alien
          ;; If its not used
          (if (not (alien-used alien))
              ;; If we need to spawn new aliens
              (progn
                (if (> new-quota 0)
                    (progn
                      (incf new-x (between 40 80))
                      (if (> new-x (- +BUFFER-W+ 60))
                          (decf new-x (- +BUFFER-W+ 60)))
                      (setf (alien-x alien) new-x)
                      (setf (alien-y alien) (between -40 -30))
                      (setf (alien-type alien) (between 0 +ALIEN-TYPE-N+))
                      (setf (alien-shot-timer alien) (between 1 99))
                      (setf (alien-blink alien) 0)
                      (setf (alien-used alien) t)
                      (cond
                        ((= (alien-type alien) +ALIEN-TYPE-BUG+) (setf (alien-life alien) 4))
                        ((= (alien-type alien) +ALIEN-TYPE-ARROW+) (setf (alien-life alien) 2))
                        ((= (alien-type alien) +ALIEN-TYPE-THICCBOI+) (setf (alien-life alien) 12)))
                      (decf new-quota 1)))
                (return-from each-alien)))
          
          ;; Move alien down
          (let ((type (alien-type alien)))
            (cond
              ((= type +ALIEN-TYPE-BUG+) (if (> (mod *FRAMES* 2) 0)
                                              (incf (alien-y alien) 1)))
              ((= type +ALIEN-TYPE-ARROW+) (incf (alien-y alien) 1))
              ((= type +ALIEN-TYPE-THICCBOI+) (if (> (mod *FRAMES* 4))
                                                  (incf (alien-y alien) 1)))))

          ;; If the alien is below bottom, then remove it
          (if (>= (alien-y alien) +BUFFER-W+)
              (progn
                (setf (alien-used alien) nil)
                (return-from each-alien)))
          
          ;; If alien is still blinking...
          (if (> (alien-blink alien) 0)
              (decf (alien-blink alien) 1))
          
          ;; If alien hit by shot
          (if (shots-collide nil (alien-x alien) (alien-y alien)
                             (elt *ALIEN-W* (alien-type alien))
                             (elt *ALIEN-H* (alien-type alien)))
              (progn
                (decf (alien-life alien) 1)
                (setf (alien-blink alien) 4)))
          
          (let ((cx (round (+ (alien-x alien) (/ (elt *ALIEN-W* (alien-type alien)) 2))))
                (cy (round (+ (alien-y alien) (/ (elt *ALIEN-H* (alien-type alien)) 2)))))
            ;; If alien has been killed
            (if (<= (alien-life alien) 0)
                (progn
                  (fx-add nil cx cy)
                  (ccase (alien-type alien)
                    (0 (incf *SCORE* 200))
                    (1 (incf *SCORE* 150))
                    (2
                     (incf *SCORE* 800)
                     (fx-add nil (- cx 10) (- cy 4))
                     (fx-add nil (+ cx 4) (+ cy 10))
                     (fx-add nil (+ cx 8) (+ cy 8))))
                  (setf (alien-used alien) nil)
                  (return-from each-alien)))
            
            ;; Reduce time alien can shoot
            (decf (alien-shot-timer alien) 1)
            
            ;; If alien can shoot this frame
            (if (= (alien-shot-timer alien) 0)
                (progn
                 (ccase (alien-type alien)
                   (0
                    (shots-add nil nil cx cy)
                    (setf (alien-shot-timer alien) 150))
                   (1
                    (shots-add nil t cx (alien-y alien))
                    (setf (alien-shot-timer alien) 80))
                   (2
                    (shots-add nil t (- cx 5) cy)
                    (shots-add nil t (+ cx 5) cy)
                    (shots-add nil t (- cx 5) (+ cy 8))
                    (shots-add nil t (+ cx 5) (+ cy 8))
                    (setf (alien-shot-timer alien) 200)))))))))))

(defun aliens-draw ()
  (dotimes (index +ALIENS-N+)
	(when (alien-used (aref *ALIENS* index))
	  (when (<= (alien-blink (aref *ALIENS* index)) 2)
		(al:draw-bitmap (aref (sprites-alien *SPRITES*) (alien-type (aref *ALIENS* index)))
						(alien-x (aref *ALIENS* index)) (alien-y (aref *ALIENS* index)) 0)))))

;; --- Stars ---

(defstruct STAR
  (y 0.0 :type float)
  (speed 0.0 :type float))

(defconstant +STARS-N+ (- (/ +BUFFER-W+ 2) 1))
(defvar *STARS* (make-array +STARS-N+ :adjustable nil))

(defun stars-init ()
  (dotimes (index +STARS-N+)
    (setf (aref *STARS* index) (make-star :y (between-f 0.0 (coerce +BUFFER-H+ 'float))
                                          :speed (between-f 0.1 1.0)))))

(defun stars-update ()
  (dotimes (index +STARS-N+)
	(incf (star-y (aref *STARS* index)) (star-speed (aref *STARS* index)))
	(when (>= (star-y (aref *STARS* index)) +BUFFER-H+)
		  (setf (star-y (aref *STARS* index)) 0.0)
		  (setf (star-speed (aref *STARS* index)) (between-f 0.1 1.0)))))

(defun stars-draw ()
  (let ((starx 1.5))
	(dotimes (index +STARS-N+)
	  (let ((l (* (star-speed (aref *STARS* index)) 0.8)))
		(al:draw-pixel starx (star-y (aref *STARS* index)) (al:map-rgb-f l l l))
		(incf starx 2)))))

;; --- HUD ---

(defvar *FONT* nil)
(defvar *SCORE-DISPLAY* 0)

(defun hud-init ()
  (setf *FONT* (al:create-builtin-font))
  (must-init *FONT* "font")
  (setf *SCORE-DISPLAY* 0))

(defun hud-deinit ()
  (al:destroy-font *FONT*))

(defun hud-update ()
  (when (= (mod *FRAMES* 2) 0)
	(dotimes (index 5)
	  (let ((diff (ash 1 index)))
		(if (<= *SCORE-DISPLAY* (- *SCORE* diff))
			(incf *SCORE-DISPLAY* diff))))))

(defun hud-draw ()
  (let ((white (al:map-rgb-f 1 1 1)))
    (al:draw-text *FONT* white 1 1 0 (format nil "~6d" *SCORE-DISPLAY*))
    (let ((spacing (1+ +LIFE-W+)))
      (dotimes (index (ship-lives *SHIP*))
        (al:draw-bitmap (sprites-life *SPRITES*) (+ 1 (* index spacing)) 10 0)))
    (if (= (ship-lives *SHIP*) 0)
        (al:draw-text *FONT*
                      white
                      (/ +BUFFER-W+ 2)
                      (/ +BUFFER-H+ 2)
    			      (foreign-enum-value 'al::align-flags :center)
                      "G A M E  O V E R"))))

;; --- Main ---

(defun main ()
  (must-init (al:init) "allegro")
  (must-init (al:install-keyboard) "keyboard")
  (must-init (al:init-font-addon) "fonts")
  (must-init (al:init-ttf-addon) "ttf")
  (must-init (al:init-image-addon) "image")
  (must-init (al:init-primitives-addon) "primitives")
  (must-init (al:install-audio) "audio")
  (must-init (al:init-acodec-addon) "audio codecs")
  (let ((timer (al:create-timer (/ 1.0 60.0)))
		(queue (al:create-event-queue)))
	(must-init timer "timer")
	(must-init queue "queue")
	(disp-init)
	(audio-init)
	(sprites-init)
	(hud-init)
	(keyboard-init)
	(fx-init)
	(shots-init)
	(ship-init)
	(aliens-init)
	(stars-init)
	(must-init (al:reserve-samples 16) "reserve samples")
	(al:register-event-source queue (al:get-keyboard-event-source))
	(al:register-event-source queue (al:get-display-event-source *DISP*))
	(al:register-event-source queue (al:get-timer-event-source timer))
	(setf *FRAMES* 0)
	(setf *SCORE* 0)
	(let ((done nil)
		  (redraw t)
		  (event (foreign-alloc '(:union al:event))))
	  (al:start-timer timer)
	  (block mainloop
		(loop
          (if (> (aref *KEY* +KEY-B+) 0)
              (break))
		  (al:wait-for-event queue event)
		  (case (foreign-slot-value event '(:union al:event) 'al::type)
			(:timer
			 (fx-update)
			 (shots-update)
			 (stars-update)
			 (ship-update)
			 (aliens-update)
			 (hud-update)
			 (if (not (= 0 (aref *KEY* +KEY-ESC+)))
				 (setf done t))
			 (setf redraw t)
			 (incf *FRAMES* 1))
			(:display-close (setf done t)))
		  (if done (return-from mainloop))
		  (keyboard-update event)
		  (if (and redraw (al:is-event-queue-empty queue))
			  (progn
				(disp-pre-draw)
				(al:clear-to-color (al:map-rgb 0 0 0))
				(stars-draw)
				(aliens-draw)
				(shots-draw)
				(fx-draw)
				(ship-draw)
				(hud-draw)
				(disp-post-draw)
				(setf redraw nil)))
		  ))
	  (foreign-free event))
	(sprites-deinit)
	(hud-deinit)
	(audio-deinit)
	(disp-deinit)
	(al:destroy-timer timer)
	(al:destroy-event-queue queue)))

