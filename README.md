# turtle-graphics
Turtle graphics for Lisp using xrender.

Commands  

System  

(INIT)  
"Sets everything up and displays the drawing window"  
(SAVE "filename)  
"Saves the surface to NAME. Doesn't add .png for you. Get a life"  
(STOP)  
"Shuts everything down."  

Movement

(FORWARD UNITS)  
  "MOVE forward UNITS amount. Draws line if TURTLE-DRAWP is T."  
(BACK UNITS)  
"Move backwards UNITS amount. Draws line if TURTLE-DRAWP is T."  
(RIGHT DEGREES)  
  "Turns to the right by DEGREES."  
(LEFT DEGREES)  
  "Turns to the left by DEGREES."  

Pen commands  

(PEN-UP)  
  "Removes pen from the surface no further drawing takes place until PEN-DOWN."  
(PEN-DOWN)  
  "Sets pen to surface so further commands will draw."  
(PEN-COLOUR R G B A)  
  "Sets pen to the given colour, premultiplying alpha for xrender."  
(PEN-WIDTH REAL)  
  "Sets pen to WIDTH which accepts pixel fractions."  


Location  

(CENTER)  
	"Resets turtles starting location and heading, leaving surface intact."  
(MOVE-TO X Y)  
	"Moves turtle to given location without drawing or changing heading."  
(HEADING)  
	"Returns the turtles heading in degrees."  
(LOCATION)  
	"Returns the absolute coordinates of turtles location."  

Background  

(CLEAR)  
	"Clears the surface to background colour. Does not change location or heading."  
(RESET)  
	"Clears surface, recenters turtle and zeros heading."  

Procedure macros  

(REPEAT TIMES &rest COMMANDS)   
	"Exposes REPEAT-COUNTER to the contents of the macro. TIMES may be an integer or :FOREVER."  
(TO NAME VARS &BODY)  
	"Thin wrapper around DEFUN, VARS accepts a regular lambda list"  

EXAMPLES  

Making a procedure  

    (to square (size)  
      (repeat 4  
        (forward size)
        (right 90)))

Using a procedure  

    (repeat 10
      (square (* repeat-counter 20))
      (right 36))

Result
![example 1](https://imgur.com/a/SIaGpVt)
