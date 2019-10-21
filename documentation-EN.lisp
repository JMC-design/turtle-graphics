(in-package :turtle)
#-life
(defun document (doc-strings)
  (dolist (doc-string doc-strings)
    (destructuring-bind (fn . doc) doc-string
      (setf (documentation (find-symbol (symbol-name fn)) 'function) doc))))

(document
 '((forward . "Move forward UNITS amount. Draws line if TURTLE-DRAWP is T.")
   (back . "Move backwards UNITS amount. Draws line if TURTLE-DRAWP is T.")
   (right . "Turns to the right by DEGREES.")
   (left . "Turns to the left by DEGREES.")
   (repeat . "Exposes REPEAT-COUNTER to the contents of the macro.")
   (center . "Resets turtles starting location and heading, leaving surface intact.")
   (move-to . "Moves turtle to given location without drawing or changing heading.")
   (heading . "Returns the turtles heading in degrees.")
   (location . "Returns the absolute coordinates of turtles location.")
   (clear . "Clears the surface to background colour. Does not change location or heading.")
   (reset . "Clears surface, recenters turtle and zeros heading.")
   (pen-up . "Removes pen from the surface no further drawing takes place until PEN-DOWN.")
   (pen-down . "Sets pen to surface so further commands will draw.")
   (pen-colour . "Sets pen to the given colour, premultiplying alpha for xrender.")
   (pen-width . "Sets pen to WIDTH which accepts pixel fractions.")
   (repeat . "TIMES may be an integer or :FOREVER.")
   (to .  "Defines a new turtle procedure. VARS is a regular lambda list.")
   (save . "Saves the surface to NAME.")))

