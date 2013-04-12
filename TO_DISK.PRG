*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа копирования списка файлов.                   ║
*║             ( Определенный источник - указывам куда )                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE To_Disk
PARAMETER file_list, def_pth && массив с именами файлов и путь или имя файла
                             &&   по умолчанию (последний необязательно)

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

PRIVATE ex, nm_w, nm_1, drv_1, mss, w, c_sav
PRIVATE nav_dir      && Навигационный указатель
PRIVATE is_fdd       && Дискета ли?
PRIVATE mode         && Список или файл
PRIVATE dst_name     && имя файла, в который идет копирование

                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = IIF(TYPE("def_pth") = "C", PADR(def_pth,40), SPACE(40) )
mode = IIF(TYPE("file_list(1)") = "C", "list", "file")

*------------------------------------------------------------------------

*           Куда пишем данные?
*┌────────────────────────────────────────────────┐
*│ ┌─ Укажите полный путь и имя файла ──────────┐ │
*│ │  ....:....1....:....2....:....3....:....4  │ │
*│ └────────────────────────────────────────────┘ │
*│        < OK Ctrl-W > < Отказаться Esc >        │
*└────────────────────────────────────────────────┘

*
*     Определяем сколько требуется места.
*
PRIVATE st_nead, st_free, st_hole, f_qnt, i, pth_w, ret_p
ret_p = ""
st_nead = 0
IF mode = "list"
  f_qnt = ALEN(file_list)
  DIMENSION file_list(f_qnt)
  FOR i = 1 TO f_qnt
    st_nead = st_nead+File_Size(file_list(i))
  ENDFOR
ELSE
  st_nead = File_Size(file_list)
ENDIF

STORE 0 TO st_free, st_hole

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 52, "Куда пишем данные?"

DO Sun_Bord WITH 2, 3, 4, 48
@ 2, 5 SAY " Укажите полный путь "+IIF(mode = "file", "и имя файла ","")

c_sav = SET("CURSOR") = "ON"
nav_dir = "INPUT"
DO WHILE .T.
  DO CASE

  CASE nav_dir = "INPUT"
  SET CURSOR ON
    *-------------------------------------------------------------------
    *      Ввод полей бланка
    *
    SHOW WINDOW (win_name)
    ACTIVATE WINDOW (win_name)
    @ 3,  6 GET nm_w
    @ 5, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

    READ CYCLE
    HIDE WINDOW (win_name)
    ACTIVATE SCREEN
    SET CURSOR OFF
    IF ex = 2
      nav_dir = "EXIT"    && Вылетаем
      LOOP
    ENDIF

    *
    *  "Причесываем" то, что ввели...
    *
    nm_1 = ALLTRIM(nm_w)
    IF mode = "file"
      i = MAX(RAT(":", nm_1), RAT("\", nm_1))
      IF LEN(nm_1) > i
        dst_name = SUBSTR(nm_1, i+1)
        nm_1 = LEFT(nm_1, i)
      ELSE
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Вы так и не указали имя файла!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        LOOP
      ENDIF
    ENDIF
    IF LEN(nm_1) > 0
      IF RIGHT(nm_1,1) # "\"
        nm_1 = nm_1+"\"
      ENDIF
    ELSE
      nm_1 = nm_1+"\"
    ENDIF

    *
    *   Определяем драйв - в жизни пригодится...
    *
    IF LEN(nm_1) > 2
      IF SUBSTR(nm_1,2,1) = ":"
        drv_1 = UPPER(LEFT(nm_1,2))
      ELSE
        drv_1 = SET("DEFAULT")
      ENDIF
    ELSE
      drv_1 = SET("DEFAULT")
    ENDIF
    
    *
    *  А дискета ли?! Это очччень важно!
    *
    is_fdd = INLIST(drv_1, "A:", "B:")
    
    nav_dir = "FDD_TEST"

  CASE nav_dir = "FDD_TEST"
    *------------------------------------------------------------------------
    *      Проверяем, что с дискетой...
    *
    nav_dir = "TRUE_PATH"
    IF is_fdd
      w = drv_1
      CALL IsDisk WITH w
      IF w # drv_1
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"А есть ли на Вашем компьютере драйв "+drv_1+" и установлена ли на нем дискета?"
        mss(3) = CHR(0)+"   И как поступим дальше?[ Вставляем дискету ][ Меняем путь ][ Отказываемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "FDD_TEST"
        CASE w = 2
          nav_dir = "INPUT"
        CASE w = 3
          nav_dir = "EXIT"
        ENDCASE
      ENDIF
    ENDIF

  CASE nav_dir = "TRUE_PATH"
    *------------------------------------------------------------------------
    *      Проверяем, корректность пути...
    *
    w = LEFT(nm_1, LEN(nm_1)-1)
    nav_dir = "HOLE_SPACE"
    IF LEN(w) > 0 .AND. (LEN(w) # 2 .OR. RIGHT(w,1) # ":")
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"Вы указали некорректный путь!"
        mss(3) = CHR(0)+"  Что делаем?[ Меняем путь ][ Отказываемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "INPUT"
        CASE w = 2
          nav_dir = "EXIT"
        ENDCASE
      ENDIF
    ENDIF

  CASE nav_dir = "HOLE_SPACE"
    *------------------------------------------------------------------------
    *      А достаточно ли велик диск вообще?...
    *
    nav_dir = "FREE_SPACE"
    DO Disk_Space WITH drv_1, st_free, st_hole
    IF st_nead > st_hole
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"На драйве "+drv_1+" ВООБЩЕ нет столько места, сколько требуется!"
        mss(3) = CHR(0)+"  И как поступим дальше?[ Сменим путь ][ Откажемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "INPUT"
        CASE w = 2
          nav_dir = "EXIT"
        ENDCASE
      ENDIF
    ENDIF

  CASE nav_dir = "FREE_SPACE"
    *------------------------------------------------------------------------
    *      А достаточно ли свободного места на диске?...
    *
    nav_dir = "COPY!"
    IF st_nead > st_free
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"На драйве "+drv_1+" нет свободного места, сколько требуется!"
        mss(3) = CHR(0)+"   Как поступим дальше?[ Удалим кое-что... ][ Сменим путь ][ Откажемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "CLEAR"
        CASE w = 2
          nav_dir = "INPUT"
        CASE w = 3
          nav_dir = "EXIT"
        ENDCASE
      ENDIF
    ENDIF

  CASE nav_dir = "CLEAR"
    *------------------------------------------------------------------------
    *      Очищаем...
    *
    pth_w = nm_1
    i = Fdd_Dir(pth_w, st_nead)
    DO WHILE .NOT. EMPTY(i)
      DELETE FILE (i)
      i = Fdd_Dir(pth_w, st_nead)
    ENDDO
    nav_dir = "FREE_SPACE"

  CASE nav_dir = "COPY!"
    *------------------------------------------------------------------------
    *      Копируем!
    *
    nav_dir = "EXIT"
    PRIVATE j, sour, dest
    
    dest = FULLPATH(nm_1)
    IF mode = "list"
      FOR i = 1 TO f_qnt
        j = MAX(RAT("\",file_list(i)), RAT(":",file_list(i)))+1
        j = SUBSTR(file_list(i),j)

        sour = FULLPATH(file_list(i))
        IF dest+UPPER(j) = sour   && Попытка копировать файл на него же
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"Файл "+j+" не скопирован!"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          LOOP
        ENDIF

        IF File_O(nm_1+j)
          DIMENSION mss(4)
          mss(1) = ""
          mss(2) = CHR(0)+"Файл "+j+" уже есть!"
          mss(3) = CHR(0)+"  Как с ним поступим? [ Обновим ] [ Оставим ]"
          mss(4) = ""
          IF Out_Mess(7, "mss") = 2
            LOOP
          ENDIF
          DELETE FILE (nm_1+j)
        ENDIF

        DO Wt_Mess WITH "Пишем "+j
        COPY FILE (file_list(i)) TO (nm_1+j)
        DO Wt_Mess
      ENDFOR
      ret_p = nm_1
    ELSE
      j = MAX(RAT("\",file_list), RAT(":",file_list))+1
      j = SUBSTR(file_list,j)

      sour = FULLPATH(file_list)
      IF dest+UPPER(j) = sour+UPPER(dst_name)  && Попытка копировать файл на него же
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Файл "+dst_name+" не записан!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        EXIT
      ENDIF

      IF File_O(nm_1+dst_name)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"Файл "+nm_1+dst_name+" уже есть!"
        mss(3) = CHR(0)+"  Как с ним поступим? [ Обновим ] [ Оставим ]"
        mss(4) = ""
        IF Out_Mess(7, "mss") = 2
          ret_p = nm_1
          EXIT
        ENDIF
        DELETE FILE (nm_1+dst_name)
      ENDIF

      DO Wt_Mess WITH "Пишем "+nm_1+dst_name
      COPY FILE (file_list) TO (nm_1+dst_name)
      DO Wt_Mess
      ret_p = nm_1
    ENDIF

  OTHERWISE
    EXIT
  ENDCASE

ENDDO
*--------------------------------------------------------------------------

IF c_sav
  SET CURSOR ON
ELSE
  SET CURSOR OFF
ENDIF
POP KEY
RELEASE WINDOW (win_name)
RETURN ret_p
