PROCEDURE Seek_Sh
PARAMETER del_sp
PRIVATE extent, pth, is_text, ex, shb, l0, c0, is_corr
PRIVATE nLin
PUBLIC lwr
extent  = "PRG"
pth     = SPACE(50)
is_text = .T.
shb     = ""
l0 = FLOOR((WROWS()-9)/2)
c0 = FLOOR((WCOLS()-58)/2)
ex = 1
is_corr = .F.
*
*┌──────────────────────────────────────────────────────┐
*│            [ ] Текст       Расширение ...            │
*│┌─ Путь ─────────────────────────────────────────────┐│
*││ ....:....1....:....2....:....3....:....4....:....5 ││
*│└────────────────────────────────────────────────────┘│
*│    Шаблон ....:....1....:....2....:....3....:....4   │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

CLEAR
DEFINE WINDOW W_Get FROM l0, c0 TO l0+8, c0+57 NONE SHADOW
ACTIVATE WINDOW W_Get
@ 0,  0 SAY PADC("Поиск шаблона", WCOLS()) COLOR SCHEME 5
@ 1,  1 TO WROWS()-1, WCOLS()-2
@ 3,  2 TO 5, WCOLS()-3
@ 3,  4 SAY " Путь "
@ 2, 14 GET is_text PICTURE "@*C Текст"
@ 2, 30 SAY "Расширение" GET extent
@ 4,  4 GET pth
@ 6,  6 SAY "Шаблон"
@ 6, 13 EDIT shb SIZE 1, 40
@ 7, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE
RELEASE WINDOW W_Get

IF ex = 2 .OR. shb == ""
  RETURN
ENDIF

PRIVATE n_fls, l_fls, buff1, buff2, i, f_numb, fnd

pth = ALLTRIM(pth)
n_fls = ADIR(l_fls, pth+"*."+extent)
IF n_fls = 0
  WAIT WINDOW pth+"*."+extent+" Не найдено ни одного файла!"
  RETURN
ENDIF

DO Set_Lwr
shb = SYS(15, lwr, shb)

IF is_text
  FOR i = 1 TO n_fls
    fnd = .F.
    f_numb = FOPEN(pth+l_fls(i,1))
    IF f_numb <= 0
      WAIT WINDOW "Неудача при открытии файла "+l_fls(i,1)
      LOOP
    ELSE
*      WAIT WINDOW l_fls(i,1) NOWAIT
      ? l_fls(i,1)	&&&&&&
    ENDIF
    nLin = 0
    DO WHILE .NOT. FEOF(f_numb)
      nLin = nLin+1
      buff1 = FGETS(f_numb, 300)
      buff1 = SYS(15, lwr, buff1)
      IF del_sp
        buff1 = CHRTRAN(buff1, " ", "")
        buff1 = CHRTRAN(buff1, CHR(9), "")
      ENDIF
      IF shb $ buff1
        fnd = .T.
        EXIT
      ENDIF
    ENDDO
    = FCLOSE(f_numb)
    IF fnd
      IF del_sp
*        WAIT WINDOW l_fls(i,1)
        append blank
        replace p_name with l_fls(i,1)
*        MODIFY COMMAND (pth+l_fls(i,1))
      ELSE
        WAIT WIND pth+l_fls(i,1)
*        KEYBOARD "{Ctrl+Home}{Ctrl+F}"+shb+"{Ctrl+W}"
        MODIFY COMMAND (pth+l_fls(i,1))
      ENDIF
    ENDIF
  ENDFOR
ELSE
  FOR i = 1 TO n_fls
    f_numb = FOPEN(pth+l_fls(i,1))
    IF f_numb <= 0
      WAIT WINDOW "Неудача при открытии файла "+l_fls(i,1)
      LOOP
    ELSE
      WAIT WINDOW l_fls(i,1) NOWAIT
      ? l_fls(i,1) &&&&&&&&&&&&&&&&&&&
    ENDIF
    buff1 = ""
    DO WHILE .NOT. FEOF(f_numb)
      buff2 = FREAD(f_numb, 500)
      buff2 = SYS(15, lwr, buff2)
      IF shb $ buff1+buff2
        WAIT WINDOW "Шаблон найден в файле "+l_fls(i,1)
        EXIT
      ENDIF
      buff1 = buff2
    ENDDO
    = FCLOSE(f_numb)
  ENDFOR
ENDIF

RETURN

PROCEDURE set_lwr
PRIVATE j
ERASE lwr
PUBLIC lwr
lwr = ""
For j = 1 to 255
  lwr = lwr+CHR(j)
ENDFOR
lwr = stuff(lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
lwr = stuff(lwr,128,32,"абвгдежзийклмнопрстуфхцчшщъыьэюя")
RETURN
