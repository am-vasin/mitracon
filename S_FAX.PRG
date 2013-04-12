*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл S_Fax        Разработчик Андрей Васин           15.09.98 13:20:34 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Программа отсылки факса.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE S_Fax
PARAMETER s_name, c_code

PRIVATE w_code, w_phone, w_to, w_from, cus_w
PRIVATE w_subj, w_name, w_exitto
PRIVATE s_sav, i

s_sav = SELECT()
w_phone = SPACE(20)
w_to    = SPACE(25)
cus_w = IIF(TYPE("c_code") = "N", MAX(c_code,0), 0)
SELECT 0
IF cus_w # 0
  USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS CLI000 AGAIN
  IF SEEK(cus_w)
    w_phone = FAX
    w_to    = LEFT(ALLTRIM(NAME),25)
  ENDIF
ENDIF

USE (send_log+"SENDMESS")
w_code  = 0
w_from  = PADR("ЗАО КОМПЭЛ", 50)
w_subj  = PADR(ALLTRIM(s_name), 64)
w_name  = ""
w_exitto= IIF("(" $ w_phone, 2, 1)

IF .NOT. Fax_Scr()
  USE
  SELECT (s_sav)
  RETURN
ENDIF
*
*  Формируем имя файла
*
w_name = SUBSTR("ABCDEFJHIJKLMNOPQRSTUVWXYZ", YEAR(DATE())-1997,1)+ ;
         SUBSTR("0123456789AB", MONTH(DATE()),1)+ ;
         RIGHT(STR(100+DAY(DATE()),3),2)
SELECT 0
USE (base_path+"PARMS")
DO WHILE .NOT. LOCK()
ENDDO
IF LEFT(LAST_SEND,4) = w_name
  w_name = w_name+RIGHT(STR(10000+VAL(RIGHT(LAST_SEND,4))+1,5),4)
ELSE
  w_name = w_name+"0000"
ENDIF
REPLACE LAST_SEND WITH w_name
UNLOCK
USE (send_log+"MATERS")
DO WHILE .NOT. LOCK()
ENDDO
w_code = SEND_MT
REPLACE SEND_MT WITH SEND_MT+1
UNLOCK
USE
w_name = send_path+w_name+".TXT"
DO Copy_D_W WITH f_nam, w_name
SELECT SENDMESS

DO CASE
CASE w_exitto = 1
  w_exitto = " "
CASE w_exitto = 2
  w_exitto = "1"
CASE w_exitto = 3
  w_exitto = "2"
ENDCASE

APPEND BLANK
REPLACE CODE    WITH w_code,  ;
        PHONE   WITH w_phone, ;
        TO      WITH w_to, ;
        FROM    WITH w_from, ;
        SUBJECT WITH w_subj, ;
        NAME    WITH w_name, ;
        EXITTO  WITH w_exitto, ;
        FAX     WITH "*", ;
        DATE_ON WITH DATE(), ;
        WHO     WITH user, ;
        WHERE   WITH arm
USE
SELECT (s_sav)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║            Интерактивное заполнение информации для отсылки.            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 15.09.98 13:33:42 ═╝
PROCEDURE Fax_Scr
*
*  Переменные состояния для навигации
*

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Тема сообщения ───────────────────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!....:....!....│ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ ┌─ От кого ──────────────────────────────────────────────────────┐ │
*│ │       ....:....!....:....!....:....!....:....!....:....!       │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ ┌─ Кому ─────────────────────────────────────────────────────────┐ │
*│ │                   ....:....!....:....!....                     │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ ┌─ Факс ─────────────────────────────────────────────────────────┐ │
*│ │       ( ) Городской  ( ) Междугородн.  ( ) Международн.        │ │
*│ │                   Номер ....:....!....:....!                   │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│                 < OK Ctrl-W > < Отказаться Esc >                   │
*└────────────────────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 17, 72, "Отсылка факса"
DO Sun_Bord WITH  2, 3,  4, 68, " Тема сообщения "
DO Sun_Bord WITH  5, 3,  7, 68, " От кого "
DO Sun_Bord WITH  8, 3, 10, 68, " Кому "
DO Sun_Bord WITH 11, 3, 14, 68, " Факс "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  9, 23 GET w_to
@ 12, 11 GET w_exitto PICTURE "@*RH Городской ;Междугородн. ;Международн."
@ 13, 23 SAY "Номер" GET w_phone
@ 15, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
@  3,  4 GET w_subj
@  6, 11 GET w_from
READ CYCLE VALID Tst_Phone()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Проверка корректности формы.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 15.09.98 16:56:23 ═╝
PROCEDURE Tst_Phone
PRIVATE p_w, i, c

IF ex = 2
  RETURN .T.
ENDIF
p_w = ""
FOR i = 1 TO LEN(w_phone)
  c = SUBSTR(w_phone,i,1)
  IF ISDIGIT(c)
    p_w = p_w+c
  ENDIF
ENDFOR

IF EMPTY(p_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Номер факса указан некорректно или не указан вовсе!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
w_phone = p_w

RETURN .T.
