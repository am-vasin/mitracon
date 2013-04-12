*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Do_Dist      Разработчик Андрей Васин           21.11.97 12:31:09 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Программа подготовки демонстрационной версии.              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Do_Dist
PRIVATE fdd, s_nom, n_i, cus, sto_typ, dist_dir, fox_dir, base_dir
PRIVATE _1, d_s, d_s_s, s1, s, i, nf, scrn
PRIVATE sav_dir, sav_drv

sav_drv = SET("DEFAULT")  && текущая директория ( плюс драйв! )
sav_dir = CURDIR()  && текущая директория ( плюс драйв! )

SET DEFAULT TO (SYS(2004)) && Текущая директория - директория FoxPro и программ
s = "! "+SET("DEFAULT")
&s
s = "! CD "+CURDIR()
s = LEFT(s,LEN(s)-1)
&s

prmo_path = sav_dir+"PRMS\"

SET PROCEDURE TO MONITOR
DO Set_Color        && установка цветов
DO Set_Proc         && установка параметров среды FoxPro
@ 0, 0, WROWS()-1, WCOLS()-1 BOX REPLICATE("░",9)
DO Sys_Tit
SAVE SCREEN TO scrn
LOAD ISDISK
SET PROCEDURE TO MONITOR

dist_dir = CURDIR()+"DIST\"
fox_dir  = dist_dir+"FOX\"
base_dir = dist_dir+"BASE\"
  
fdd   = "A:"
s_nom = PADR(SYS(3),8)
n_i   = 0
cus   = SPACE(40)
sto_typ = "СКЛАД        "

*************************************************************************
*     Заполнение таблицы
*************************************************************************
IF .NOT. S_Form()
  s = "! "+sav_drv
  &s
  s = "! CD "+sav_dir
  &s
  QUIT
ENDIF
SET CURSOR OFF

*************************************************************************
*     Коррекция кода в части переменных, связанных с регистрацией
*************************************************************************
_1 = n_i
_1 = RIGHT(STR(1000000+VAL(ALLTRIM(SYS(2))),7),6)+RIGHT(STR(100+_1,3),2)
DO SetPrice WITH s_nom, _1
SAVE ALL LIKE _1 TO (fdd+"sub_inst.bat")

d_s = Fdd_Ser(fdd)
d_s_s = d_s
DO ClrPrice WITH s_nom, d_s

pos = -2
nf = FOPEN(dist_dir+"monitor.prg",2)
s = ""
DO WHILE .NOT. " Подстановка 1" $ s .AND. .NOT. FEOF(nf)
  pos = pos+LEN(s)+2
  s = FGETS(nf,200)
ENDDO

IF FEOF(nf)
  WAIT WINDOW "Неудача при поиске подстановки 1"
  s = "! "+sav_drv
  &s
  s = "! CD "+sav_dir
  &s
  QUIT
ENDIF

s1 = 'rg_nom = "'+s_nom+'"'
s = s1+SUBSTR(s,LEN(s1)+1)
= FSEEK(nf,pos)
= FPUTS(nf,s)

s1 = ""
FOR i = 1 TO LEN(d_s)
  s = ASC(SUBSTR(d_s,i,1))
  s = "CHR("+STR(s,3)+")"
  s = IIF(EMPTY(s1), "", "+")+s
  s1 = s1+s
ENDFOR
= FSEEK(nf,0)
s = ""
pos = -2
DO WHILE .NOT. " Подстановка 2" $ s .AND. .NOT. FEOF(nf)
  pos = pos+LEN(s)+2
  s = FGETS(nf,200)
ENDDO

IF FEOF(nf)
  WAIT WINDOW "Неудача при поиске подстановки 2"
  s = "! "+sav_drv
  &s
  s = "! CD "+sav_dir
  &s
  QUIT
ENDIF

s = s1+SUBSTR(s,LEN(s1)+1)
= FSEEK(nf,pos)
= FPUTS(nf,s)
= FCLOSE(nf)

DELETE FILE (dist_dir+"STORE.APP")


*************************************************************************
*     Строим проект
*************************************************************************
DO W_Wind WITH "Строим проект"
s = "BUILD APP "+dist_dir+"STORE FROM "+dist_dir+"STORE"
&s
*BUILD APP (dist_dir+"STORE") FROM (dist_dir+"STORE")
KEYBOARD " "    && попытка очистить окна...
READ
SET CURSOR OFF

s = "RUN /300K ARJ A "+fdd+"STORE "+dist_dir+"STORE.APP > Nul:"
&s


*************************************************************************
*     Подготовка файлов...
*************************************************************************
USE (base_dir+"PARMS") EXCLUSIVE
ZAP
APPEND BLANK
sto_typ = ALLTRIM(sto_typ)
DO CASE
CASE sto_typ = "Склад КОМПЭЛ"

  REPLACE MODE WITH "main C"

CASE sto_typ = "Филиал КОМПЭЛ"

  REPLACE MODE WITH "filial C"

CASE sto_typ = "СКЛАД"

  REPLACE MODE WITH "main O"

ENDCASE
USE

*************************************************************************
*     Запись на дискету базы данных
*************************************************************************
DO W_Wind WITH "База данных."

DELETE FILE (base_dir+"BASE.ARJ")

s = "RUN /300K ARJ A -E "+fdd+"BASE "+base_dir+"BAS_LIST.DBF > Nul:"
&s
s = "RUN /300K ARJ A -E "+fdd+"BASE "+base_dir+"BAS_LIST.FPT > Nul:"
&s
s = "RUN /300K ARJ A -E "+fdd+"BASE "+base_dir+"PARMS.DBF > Nul:"
&s

*COPY FILE (base_dir+"BASE.ARJ") TO (fdd+"BASE.ARJ")
COPY FILE (dist_dir+"FDD_STAT.EXE") TO (fdd+"FDD_STAT.EXE")
COPY FILE (dist_dir+"ARJ.EXE") TO (fdd+"ARJ.EXE")

DO W_Wind WITH "Модули FoxPro."

COPY FILE (fox_dir+"INSTALL.BAT") TO (fdd+"INSTALL.BAT")
COPY FILE (fox_dir+"FOXPROA.ARJ") TO (fdd+"FOXPRO.ARJ")

*USE SAL_LOG
*APPEND BLANK
*REPLACE N_DISK   WITH d_s_s,   ;
*        VERSION  WITH "1.0",   ;
*        DT       WITH DATE(),  ;
*        TM       WITH TIME(),  ;
*        N_INST   WITH n_i,     ;
*        SER_NO   WITH s_nom,   ;
*        CUSTOMER WITH cus
*USE

RESTORE SCREEN FROM scrn
*@ 0, 0, WROWS()-1, WCOLS()-1 BOX REPLICATE("░",9)

SET PROCEDURE TO MONITOR

SET CURSOR ON
DO Copy_Doc

s = "! "+sav_drv
&s
s = "! CD "+sav_dir
&s
QUIT

*╔══════════════════════════════════════════════════════════════════════╗
*║                                                                      ║
*║         Программа привязки приложения к HARD диску.                  ║
*║                                                                      ║
*╚══════════════════════════════════════════════════════════════════════╝
PROCEDURE ClrPrice
PARAMETERS s1, s2

PRIVATE sw, i, n
sw = ""
n = LEN(s2)
DO WHILE LEN(sw) < n
  sw = sw+s1
ENDDO

s2 = STUFF(s2,1,1,CHR(MOD(ASC(SUBSTR(s2,1,1))-ASC(RIGHT(s2,1)),256)))

FOR i = n TO 1 STEP -1
  s2 = STUFF(s2,i,1,CHR(MOD(ASC(SUBSTR(s2,i,1))-ASC(SUBSTR(sw,i,1))-IIF(i#1, ASC(SUBSTR(s2,i-1,1)), 0)+1024,256)))
ENDFOR

RETURN

PROCEDURE SetPrice
PARAMETERS s1, s2

PRIVATE i, sw, n, cp
n = LEN(s2)
sw = ""
DO WHILE LEN(sw) < n
  sw = sw+s1
ENDDO
cp = 0
FOR i = 1 TO n
  cp = MOD(ASC(SUBSTR(sw,i,1))+ASC(SUBSTR(s2,i,1))+cp,256)
  s2 = STUFF(s2,i,1,CHR(cp))
ENDFOR
s2 = STUFF(s2,1,1,CHR(MOD(ASC(SUBSTR(s2,1,1))+cp,256)))

RETURN

*╔═══════════════════════════════════════════════════════════════════╗
*║                                                                   ║
*║        Программа получения серийного номера диска.                ║
*║                                                                   ║
*╚═══════════════════════════════════════════════════════════════════╝
PROCEDURE Fdd_Ser
PARAMETER drive

PRIVATE s_w, f_name, n_f, s, i, pth

pth = SET("DEFAULT")+CURDIR()

f_name = SYS(3)

IF LEN(drive) = 1
  s_w = "! vol "+drive+": > "+pth+f_name+"."
ELSE
  s_w = "! vol "+drive+" > "+pth+f_name+"."
ENDIF

&s_w

n_f = FOPEN(pth+f_name+".")

s = ""

DO WHILE .T.
  s_w = FGETS(n_f)
  i = LEN(s_w)
  IF i > 8
    IF SUBSTR(s_w, i-4, 1) = "-"
      s = RIGHT(s_w,9)
    ENDIF
  ENDIF
  IF FEOF(n_f)
    EXIT
  ENDIF
*  i = AT("Serial Number is", s_w)
*  IF i > 0
*    s = ALLTRIM(SUBSTR(s_w,i+16))
*    EXIT
*  ENDIF
ENDDO

= FCLOSE(n_f)
DELETE FILE (f_name+".")

RETURN s

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Заполнение бланка.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.11.97 14:18:51 ═╝
PROCEDURE S_Form
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

*┌──────────────────────────────────────────────┐
*│           ┌─ Диск ──────────────┐            │
*│           │   ( ) A:   ( ) B:   │            │
*│           └─────────────────────┘            │
*│        Регистрационный номер 99999999        │
*│ ┌─ Клиент ─────────────────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....! │ │
*│ └──────────────────────────────────────────┘ │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, fdd_sw

ex     = 1
fdd_sw = 1
*fdd   = "A:"
*s_nom = PADR(SYS(3),8)
*cus   = SPACE(40)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
*DO Prp_Nav_2
DO D_Win_N WITH 11, 50, "Заполните бланк"
DO Sun_Bord WITH 2, 13, 4, 37, " Диск "
DO Sun_Bord WITH 6, 3, 8, 46, " Клиент "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 18 GET fdd_sw PICTURE "@*RH A:  ;B:"
@ 5, 10 SAY "Регистрационный номер" GET s_nom
@ 7,  5 GET cus
@ 9,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Fdd(1)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Проверка дискеты.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.11.97 14:48:59 ═╝
PROCEDURE Tst_Fdd
PARAMETER mode
PRIVATE drv, aaa

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

drv = IIF(fdd_sw = 1, "A", "B")

CALL IsDisk WITH drv

IF drv # IIF(fdd_sw = 1, "A", "B")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не установили дискету!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
fdd = IIF(fdd_sw = 1, "A:", "B:")

IF mode = 1
  IF EMPTY(Fdd_Ser(fdd))
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Дискету надо отформатировать!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF

  IF ADIR(aaa, fdd+"\*.*","DHS") # 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Дискету надо отформатировать!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Копирование файла документации.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.11.97 14:18:51 ═╝
PROCEDURE Copy_Doc
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

*┌──────────────────────────────────────────────┐
*│           ┌─ Диск ──────────────┐            │
*│           │   ( ) A:   ( ) B:   │            │
*│           └─────────────────────┘            │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, fdd_sw

ex     = 1
fdd_sw = 1
*fdd   = "A:"
*s_nom = PADR(SYS(3),8)
*cus   = SPACE(40)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
*DO Prp_Nav_2
DO D_Win_N WITH 7, 50, "Куда пишем докуметацию"
DO Sun_Bord WITH 2, 13, 4, 37, " Диск "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 18 GET fdd_sw PICTURE "@*RH A:  ;B:"
@ 5,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Fdd(2)

POP KEY
RELEASE WINDOW (win_name)
IF ex = 1
  SET CURSOR OFF
  DO W_Wind WITH "Записываем инструкцию"
  COPY FILE (base_dir+"MANUAL.TXT") TO (fdd+"MANUAL.TXT")
ENDIF
*--------------------------------------------------------------------------

RETURN

***************************************************************************
PROCEDURE W_Wind
PARAMETER mss_
PRIVATE wd, c0, l0, i

wd = LEN(mss_)+8

l0 = CEILING((WROWS()-5)/2)
c0 = CEILING((WCOLS()-wd)/2)

RESTORE SCREEN FROM scrn
*@ 0, 0, WROWS()-1, WCOLS()-1 BOX REPLICATE("░",9)
FOR i = 1 TO 5
  @ l0+i-1, c0 SAY SPACE(wd) COLOR SCHEME 5
ENDFOR
@ l0+2, c0+3 SAY mss_ COLOR SCHEME 5

RETURN

***************************************************************************
PROCEDURE Sys_Tit
PRIVATE l0, c0, i
DIMENSION s000(8)
s000(1) = ;
"██████░██████░░██░░░██░░██████░░██████░░░░░█████░░██░░░██░░██░░░██░░░██████░"
s000(2) = ;
"██     ██     ░██ ░░██ ░██     ░██   ██░░░█░  ██ ░██ ░░██ ░██ ░░██ ░██   ██ "
s000(3) = ;
"██ ░░░░██ ░░░░░██ ░░██ ░██ ░░░░░██ ░░██ ░█░ ░░██ ░██ ░░██ ░██ ░░██ ░██ ░░██ "
s000(4) = ;
"██ ░░░░████░░░░███████ ░████░░░░██ ░░██ ░█ ░░░██ ░██ ░░██ ░██ ░███ ░██ ░░██ "
s000(5) = ;
"██ ░░░░██   ░░░██   ██ ░██   ░░░██████  ░███████ ░██ ░░██ ░██ █░██ ░░██████ "
s000(6) = ;
"██ ░░░░██ ░░░░░██ ░░██ ░██ ░░░░░██     ░░█    ██ ░██ ░░██ ░███░ ██ ░░░██ ██ "
s000(7) = ;
"██ ░░░░██████░░██ ░░██ ░██████░░██ ░░░░░░█ ░░░██ ░████████░██  ░██ ░██░  ██ "
s000(8) = ;
"░  ░░░░░      ░░  ░░░  ░░      ░░  ░░░░░░░ ░░░░  ░░      █ ░  ░░░  ░░  ░░░  "

l0 = FLOOR((WROWS()-ALEN(s000))/2)
c0 = FLOOR((WCOLS()-LEN(s000(1)))/2)
FOR i = 1 TO ALEN(s000)
  @ l0+i-1, c0 SAY s000(i)
ENDFOR

RETURN
