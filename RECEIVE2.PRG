*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа приема данных с основного склада.                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Receive2

PRIVATE c_name, c_code, pth_w, crs, s_sav, l, c, mss

l = .T.
IF TYPE("path_comm") # "C"
  l = .F.
ELSE
  IF EMPTY(path_comm)
    l = .F.
  ENDIF
ENDIF
IF .NOT. l
  DIMENSION src(3)
  src(1) = ""
  src(2) = CHR(0)+"Нет связи с ситемой MANAGER"
  src(3) = ""
  DO Out_Mess WITH 7, "src"
  RETURN
ENDIF

DO Prep_TD    && Создаем или очищаем транзитную директорию
pth_w = tmpo_path+"TRANSMIT\"

IF .NOT. Get_File(pth_w)
  RETURN
ENDIF

DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"Внимание! Для обновления данных требуется исключительный"
mss(3) = CHR(0)+"доступ к базе данных! Продолжаем работу? [ Да ] [ Нет ]"
mss(4) = ""
IF Out_Mess(7, "mss") = 2
  RETURN
ENDIF

ACTIVATE SCREEN
SAVE SCREEN TO s_sav

l = CEILING((WROWS()-4)/2)
c = FLOOR((WCOLS()-21)/2)
@ l+1, c+2 FILL TO l+4, c+22 COLOR (SCHEME(1,8))
@ l,   c SAY SPACE(21) COLOR SCHEME 5
@ l+1, c SAY PADC("Распаковка!",21) COLOR SCHEME 5
@ l+2, c SAY PADC("Ждем-с...",21) COLOR SCHEME 5
@ l+3, c SAY SPACE(21) COLOR SCHEME 5
c = "! ARJ E -V -Y "+pth_w+"F_STAT "+pth_w+" > nul"
&c
c = "!DEL "+pth_w+"F_STAT.A*"
&c
RESTORE SCREEN FROM s_sav

SELECT 0

PRIVATE err_lst, err_cod
DIMENSION err_lst(1)
err_lst(1) = -1
err_cod    = 0
IF File_O(pth_w+"SUBJECT.DBF")
  err_lst(1) = 1705
  err_cod    = 0
  SET EXCLUSIVE ON
  USE (path_comm+"SUBJECT") ALIAS DEST
  SET EXCLUSIVE OFF
  IF err_cod # 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Не удается получить исключительный доступ к базе данных!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF
  ZAP
  APPEND FROM (pth_w+"SUBJECT.DBF")
  USE
ENDIF

IF File_O(pth_w+"CORPUS.DBF")
  err_lst(1) = 1705
  err_cod    = 0
  SET EXCLUSIVE ON
  USE (path_comm+"CORPUS") ALIAS DEST
  SET EXCLUSIVE OFF
  IF err_cod # 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Не удается получить исключительный доступ к базе данных!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF
  ZAP
  APPEND FROM (pth_w+"CORPUS.DBF")
  USE
ENDIF

CLOSE DATABASES

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа ввода и распаковки ARJ файла.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Get_File
PARAMETER pth

PRIVATE get_pth, i, mss, mss1, nm_w, w, is_fdd

get_pth = FromDisk(pth+"F_STAT.ARJ")

IF EMPTY(get_pth)
  RETURN .F.
ENDIF

is_fdd = .F.
IF LEN(get_pth) = 3
  IF INLIST(UPPER(LEFT(get_pth,2)), "A:", "B:")
    is_fdd = .T.
  ENDIF
ENDIF
IF is_fdd
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Еще будет дискета? [ Да ] [ Нет ]"
  mss(3) = ""
  i = 1
  DO WHILE .T.
    nm_w = "F_STAT.A"+RIGHT(STR(100+i,3),2)
    IF Out_Mess(5, "mss") = 2
      EXIT
    ENDIF
    w = get_pth
    CALL IsDisk WITH w
    IF w # get_pth
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"Вы не установили дискету!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    IF .NOT. File_O(get_pth+nm_w)
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"Вы установили не ту дискету!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ELSE
  i = 1
  DO WHILE .T.
    nm_w = "F_STAT.A"+RIGHT(STR(100+i,3),2)
    w = get_pth
    IF .NOT. File_O(get_pth+nm_w)
      EXIT
    ENDIF
    DO Wt_Mess WITH "Пишем "+nm_w
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ENDIF

DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"Все считано правильно? Начинаем обработку?"
mss(3) = CHR(0)+"  [ Да ] [ Нет, откажемся! ]"
mss(4) = ""
RETURN Out_Mess(5, "mss") = 1
