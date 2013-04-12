PROCEDURE set_lwr
PRIVATE j
ERASE lwr
PUBLIC lwr
lwr = ""
For j = 1 to 255
  lwr = lwr+CHR(j)
ENDFOR
lwr = stuff(lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
lwr = stuff(lwr,128,32," ΅Ά£¤¥¦§¨©ª«¬­®―ΰαβγδεζηθικλμνξο")
RETURN
