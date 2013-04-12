*
*  Программа сравнения директорий в части DBF файлов
**************************************************************************
PROCEDURE Cmp_Dbf1

PRIVATE a_dir_1, a_dir_2, pth_1, pth_2, nf_1, nf_2, i, err00
ON ERROR err00 = ERROR()
err00 = 0
SET EXCLUSIVE OFF
CLEAR
STORE SPACE(40) TO pth_1, pth_2, p_file
pth_1 = PADR("BASE\", 40)
pth_2 = PADR("NEW_BASE\", 40)
p_file = PADR("RES.TXT", 40)
DO Set_Lwr
ex = 1
@ 10, 5 SAY "Путь к эталонной директории  " GET pth_1
@ 12, 5 SAY "Путь к проверяемой директории" GET pth_2
@ 14, 5 SAY "Фаил печати                  " GET p_file
@ 16, 18 GET ex PICTURE "@*HT \! OK (Ctrl-W) ;\? Отказаться (Esc) "

READ CYCLE
IF ex = 2
  RETURN
ENDIF

CLEAR 
@ 0, 0 SAY PADL(ALLTRIM(pth_1)+" ",40)+pth_2
pth_1 = ALLTRIM(pth_1)
pth_2 = ALLTRIM(pth_2)

nf_1 = ADIR(a_dir_1, pth_1+"*.DBF")
nf_2 = ADIR(a_dir_2, pth_2+"*.DBF")
IF nf_1 = 0 .AND. nf_2 = 0
  WAIT WINDOW "Нет DBF файлов ни в одной из директорий!"
  RETURN
ENDIF
= ASORT(a_dir_1,1)
= ASORT(a_dir_2,1)
PRIVATE cnt1, cnt2, a_dir, cnt, w1, w2

cnt1 = 1
cnt2 = 1
cnt  = 0

DO WHILE cnt1 <= nf_1 .OR. cnt2 <= nf_2

  cnt = cnt+1
  w1 = IIF(cnt1 > nf_1, REPLICATE("░", 12), PADR(a_dir_1(cnt1, 1), 12))
  w2 = IIF(cnt2 > nf_2, REPLICATE("░", 12), PADR(a_dir_2(cnt2, 1), 12))
  DIMENSION a_dir(cnt)
  DO CASE
  CASE w1 < w2
    a_dir(cnt) = w1+" "+REPLICATE("░", 12)
    cnt1 = cnt1+1
  CASE w1 > w2
    a_dir(cnt) = REPLICATE("░", 12)+" "+w2
    cnt2 = cnt2+1
  OTHERWISE
    a_dir(cnt) = w1+" "+w2
    cnt1 = cnt1+1
    cnt2 = cnt2+1
  ENDCASE
ENDDO

FOR i = 1 TO cnt
  a_dir(i) = a_dir(i)+" "+Diff0(a_dir(i), pth_1, pth_2)
ENDFOR


PRIVATE ln, wd, c0, l0, s

ln = MIN(WROWS("")-7, cnt)+2
wd = LEN(a_dir(1))+2
l0 = CEILING((WROWS("")-ln)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
ln = ln-2

wd = 1

SET PRINTER TO (ALLTRIM(p_file))
SET DEVICE TO PRINTER
FOR i = 1 TO cnt
  IF RIGHT(a_dir(i),2) # "  "
    DO P_Diff WITH a_dir(i), pth_1, pth_2
  ENDIF
ENDFOR
SET DEVICE TO SCREEN
SET PRINTER TO PRN:

RETURN

*
*    Программа формирования структуры базы данных.
**************************************************************************
PROCEDURE Dbf_Stru
PARAMETERS f_name, a_fld, a_tag

PRIVATE i

err00 = 0
USE (f_name)
IF err00 # 0
  err00 = 0
  RETURN
ENDIF

= AFIELDS(a_fld)

DIMENSION a_tag(1, 3)
STORE "" TO a_tag(1,1), a_tag(1,2), a_tag(1,3)

i = 1
DO WHILE .T.
  IF EMPTY(TAG(i))
    EXIT
  ENDIF
  DIMENSION a_tag(i, 3)
  a_tag(i, 1) = TAG(i)
  a_tag(i, 2) = SYS(14,i)
  a_tag(i, 3) = SYS(2021,i)
  i = i+1
ENDDO

USE
= ASORT(a_tag,1)

RETURN

*
*   Формирование признаков различия
**************************************************************************
PROCEDURE Diff0
PARAMETERS nms, p1, p2

PRIVATE nm, a_f1, a_t1, a_f2, a_t2, i, n, res

DIMENSION a_f1(1,4), a_t1(1,3), a_f2(1,4), a_t2(1,3)

IF "░" $ nms
  RETURN "√√"
ENDIF

nm = ALLTRIM(LEFT(nms,12))
DO Dbf_Stru WITH p1+nm, a_f1, a_t1

nm = ALLTRIM(RIGHT(nms,12))
DO Dbf_Stru WITH p2+nm, a_f2, a_t2

IF ALEN(a_f1) # ALEN(a_f2)
  nm = "√"
ELSE
  n = ALEN(a_f1,1)
  nm = " "
  FOR i = 1 TO n
    IF .NOT. a_f1(i,1) == a_f2(i,1) .OR. ;
             a_f1(i,2) #  a_f2(i,2) .OR. ;
             a_f1(i,3) #  a_f2(i,3) .OR. ;
             a_f1(i,4) #  a_f2(i,4)
      nm = "√"
      EXIT
    ENDIF
  ENDFOR
ENDIF

res = nm

IF ALEN(a_t1) # ALEN(a_t2)
  nm = "√"
ELSE
  n = ALEN(a_t1,1)
  nm = " "
  FOR i = 1 TO n
    IF .NOT. a_t1(i,1) == a_t2(i,1) .OR. ;
       .NOT. a_t1(i,2) == a_t2(i,2) .OR. ;
       .NOT. a_t1(i,3) == a_t2(i,3)
      nm = "√"
      EXIT
    ENDIF
  ENDFOR
ENDIF

RETURN res+nm

**************************************************************************
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

*
*  Печать структуры
**************************************************************************
PROCEDURE P_Diff
PARAMETERS nms, p1, p2
@ PROW()+1, 0 SAY "====================================================================="
@ PROW()+1, 0 SAY  nms
@ PROW()+1, 0 SAY  "====================================================================="
DO View_I WITH nms, p1, p2
DO View_F WITH nms, p1, p2
RETURN

*
*  Просмотр полей
**************************************************************************
PROCEDURE View_F
PARAMETERS nms, p1, p2

PRIVATE nm1, nm2, tit, arr1, arr2, arr, p, l1, l2, l, w1, w2, i

nm1 = ALLTRIM(LEFT(nms, 12))
nm2 = ALLTRIM(SUBSTR(nms, 14, 12))

IF "░" $ nm1
  DIMENSION arr1(1, 4)
  arr1(1,1) = ""
  arr1(1,2) = " "
  arr1(1,3) = 0
  arr1(1,4) = 0
  l1 = 1
ELSE
  USE (p1+nm1)
  l1 = AFIELDS(arr1)
  tit = nm1
ENDIF

IF "░" $ nm2
  DIMENSION arr2(1, 4)
  arr2(1,1) = ""
  arr2(1,2) = " "
  arr2(1,3) = 0
  arr2(1,4) = 0
  l2 = 1
ELSE
  USE (p2+nm2)
  l2 = AFIELDS(arr2)
  tit = nm2
ENDIF
USE

l = MAX(l1, l2)
DIMENSION arr(l+2)
arr(1) = "┌"+REPLICATE("─",19)+"┬"+REPLICATE("─",19)+"┐"
arr(l+2) = "└"+REPLICATE("─",19)+"┴"+REPLICATE("─",19)+"┘"
FOR i = 1 TO l
  IF i > l1
    w1 = SPACE(19)
  ELSE
    w1 = PADR(arr1(i,1),11)+arr1(i,2)+" "+  ;
         TRANSFORM(arr1(i,3), "@Z 999")+TRANSFORM(arr1(i,4), "@Z 999")
  ENDIF
  IF i > l2
    w2 = SPACE(19)
  ELSE
    w2 = PADR(arr2(i,1),11)+arr2(i,2)+" "+  ;
         TRANSFORM(arr2(i,3), "@Z 999")+TRANSFORM(arr2(i,4), "@Z 999")
  ENDIF
  arr(i+1) = "│"+w1+"│"+w2+IIF(w1=w2, "│", "│√")
ENDFOR

FOR l = 1 TO ALEN(arr)
  @ PROW()+1, 0 SAY  arr(l)
ENDFOR
RETURN

*
*  Просмотр индексов
**************************************************************************
PROCEDURE View_I
PARAMETERS nms, p1, p2

PRIVATE nm1, nm2, tit, arr1, arr2, arr, p, l1, l2, l, w1, w2, i, j, w, k

nm1 = ALLTRIM(LEFT(nms, 12))
nm2 = ALLTRIM(SUBSTR(nms, 14, 12))
DIMENSION arr1(1,3), arr2(1,3), arr(1,4)

IF .NOT. "░" $ nm1
  DO Dbf_Stru WITH p1+nm1, arr, arr1
  tit = nm1
ENDIF

IF .NOT. "░" $ nm2
  DO Dbf_Stru WITH p2+nm2, arr, arr2
  tit = nm2
ENDIF

IF EMPTY(arr1(1,1)) .AND. EMPTY(arr2(1,1))
  RETURN
ENDIF
l1 = ALEN(arr1, 1)
l2 = ALEN(arr2, 1)

l = MAX(l1, l2)

k = 0
FOR i = 1 TO l
  IF i = 1
    k = k+1
    DIMENSION arr(k)
    arr(k) = "┌"+REPLICATE("─",37)+"┬"+REPLICATE("─",37)+"┐"
  ENDIF
  IF i > l1 .OR. EMPTY(arr1(i,1))
    DIMENSION w1(1)
    w1(1) = SPACE(35)
  ELSE
    j = 1
    DIMENSION w1(j)
    w1(j) = PADR("--TAG "+arr1(i,1), 35)
    j = j+1
    DIMENSION w1(j)
    w1(j) = PADR("--Выражение", 35)
    w = arr1(i,2)
    DO WHILE .NOT. EMPTY(w)
      j = j+1
      DIMENSION w1(j)
      w1(j) = PADR(LEFT(w,35), 35)
      w = SUBSTR(w,40)
    ENDDO
    j = j+1
    DIMENSION w1(j)
    w1(j) = PADR("--Фильтр", 35)
    w = arr1(i,3)
    DO WHILE .NOT. EMPTY(w)
      j = j+1
      DIMENSION w1(j)
      w1(j) = PADR(LEFT(w,35),35)
      w = SUBSTR(w,40)
    ENDDO
  ENDIF

  IF i > l2 .OR. EMPTY(arr2(i,1))
    DIMENSION w2(1)
    w2(1) = SPACE(35)
  ELSE
    j = 1
    DIMENSION w2(j)
    w2(j) = PADR("--TAG "+arr2(i,1), 35)
    j = j+1
    DIMENSION w2(j)
    w2(j) = PADR("--Выражение", 35)
    w = arr2(i,2)
    DO WHILE .NOT. EMPTY(w)
      j = j+1
      DIMENSION w2(j)
      w2(j) = PADR(LEFT(w,35),35)
      w = SUBSTR(w,40)
    ENDDO
    j = j+1
    DIMENSION w2(j)
    w2(j) = PADR("--Фильтр", 35)
    w = arr2(i,3)
    DO WHILE .NOT. EMPTY(w)
      j = j+1
      DIMENSION w2(j)
      w2(j) = PADR(LEFT(w,35),35)
      w = SUBSTR(w,40)
    ENDDO
  ENDIF
  FOR j = 1 TO MAX(ALEN(w1), ALEN(w2))
    k = k+1
    DIMENSION arr(k)
    arr(k) = IIF(j > ALEN(w1), SPACE(35), w1(j))+ " │ "+ ;
             IIF(j > ALEN(w2), SPACE(35), w2(j))
    arr(k) = "│ "+arr(k)+IIF(LEFT(arr(k),35)#RIGHT(arr(k),35)," │█"," │")
  ENDFOR
  k = k+1
  DIMENSION arr(k)
  IF i = l
    arr(k) = "└"+REPLICATE("─",37)+"┴"+REPLICATE("─",37)+"┘"
  ELSE
    arr(k) = "├"+REPLICATE("─",37)+"┼"+REPLICATE("─",37)+"┤"
  ENDIF
ENDFOR

ln = MIN(WROWS("")-7, k)+2
wd = LEN(arr(1))+2
l0 = CEILING((WROWS("")-ln)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
ln = ln-2

wd = 1

FOR l = 1 TO ALEN(arr)
  @ PROW()+1, 0 SAY  arr(l)
ENDFOR

RETURN
