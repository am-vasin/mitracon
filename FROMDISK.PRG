*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа копирования списка файлов.                   ║
*║           ( Определенный результат - указывам откуда )                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE FromDisk
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
PRIVATE st_nead, st_free, st_hole, f_qnt, i, j, pth_w, ret_p
IF mode = "list"
  f_qnt = ALEN(file_list)
  DIMENSION file_list(f_qnt)
ENDIF
ret_p = ""

STORE 0 TO st_free, st_hole

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 52, "Где ищем данные?"

DO Sun_Bord WITH 2, 3, 4, 48
@ 2, 5 SAY " Укажите полный путь "

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
    nav_dir = "EXIST?"
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

  CASE nav_dir = "EXIST?"

    nav_dir = "COPY!"
    PRIVATE abs_list
    w = 0
    IF mode = "list"
      FOR i = 1 TO f_qnt
        IF .NOT. File_O(nm_1+F_Name(file_list(i)))
          w = w+1
          DIMENSION abs_list(w)
          abs_list(w) = ALLTRIM(F_Name(file_list(i)))
        ENDIF
      ENDFOR
    ELSE
      IF .NOT. File_O(nm_1+F_Name(file_list))
        w = 1
        DIMENSION abs_list(1)
        abs_list(1) = ALLTRIM(F_Name(file_list))
      ENDIF
    ENDIF
    IF w # 0
      DIMENSION mss(2)
      mss(1) = ""
      mss(2) = CHR(0)+"В указанной Вами директории не хватает файлов:"
      i = 1
      j = ""
      DO WHILE .T.
        IF LEN(j) > 50
          DIMENSION mss(ALEN(mss)+1)
          mss(ALEN(mss)) = CHR(0)+j
          j = ""
        ENDIF
        j = j+abs_list(i)+", "
        i = i+1
        IF i > ALEN(abs_list)
          EXIT
        ENDIF
      ENDDO
      j = LEFT(j, LEN(j)-2)
      DIMENSION mss(ALEN(mss)+2)
      mss(ALEN(mss)-1) = CHR(0)+j
      mss(ALEN(mss)) = ""
      DO Out_Mess WITH 7, "mss"
      nav_dir = "EXIT"
    ENDIF

  CASE nav_dir = "COPY!"
    *------------------------------------------------------------------------
    *      Копируем!
    *
    nav_dir = "EXIT"
    PRIVATE f_f, f_t

    IF mode = "list"
      FOR i = 1 TO f_qnt
        IF File_O(file_list(i))
          DIMENSION mss(4)
          mss(1) = ""
          mss(2) = CHR(0)+"Файл "+F_Name(file_list(i))+" уже есть!"
          mss(3) = CHR(0)+"  Как с ним поступим? [ Обновим ] [ Оставим ]"
          mss(4) = ""
          IF Out_Mess(7, "mss") = 2
            LOOP
          ENDIF
          DELETE FILE (file_list(i))
        ENDIF
        DO Disk_Space WITH LEFT(FULLPATH(file_list(i)),2), st_free, st_hole
        IF st_free < File_Size(nm_1+F_Name(file_list(i)))
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"Для файла "+F_Name(file_list(i))+" нет места!"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          EXIT
        ENDIF

        DO Wt_Mess WITH "Пишем "+F_Name(file_list(i))
        f_f = nm_1+F_Name(file_list(i))
        f_t = file_list(i)
        COPY FILE (f_f) TO (f_t)
        DO Wt_Mess
      ENDFOR
      ret_p = nm_1
    ELSE
      IF File_O(file_list)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"Файл "+F_Name(file_list)+" уже есть!"
        mss(3) = CHR(0)+"  Как с ним поступим? [ Обновим ] [ Оставим ]"
        mss(4) = ""
        IF Out_Mess(7, "mss") = 2
          ret_p = nm_1
          EXIT
        ENDIF
        DELETE FILE (file_list)
      ENDIF
      DO Disk_Space WITH LEFT(FULLPATH(file_list),2), st_free, st_hole
      IF st_free < File_Size(nm_1+F_Name(file_list))
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Для файла "+F_Name(file_list)+" нет места!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        EXIT
      ENDIF

      DO Wt_Mess WITH "Пишем "+F_Name(file_list)
      f_f = nm_1+F_Name(file_list)
      f_t = file_list
      COPY FILE (f_f) TO (f_t)
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

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║      Программа выделения имени файла из полной ( или неполной )       ║
*║                             спецификации.                             ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE F_Name
PARAMETERS full_sp

PRIVATE i000

i000 = MAX(RAT("\", full_sp), RAT(":", full_sp))
IF i000 = LEN(full_sp)
  RETURN ""
ENDIF
RETURN SUBSTR(full_sp,i000+1)
