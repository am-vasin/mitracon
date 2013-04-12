*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Copy_A       Разработчик Андрей Васин           18.03.97 22:14:57 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Программа копирования архива.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Copy_A
PARAMETER f_from, f_to  && Откуда-куда: пути и имена файлов (без расширений)

PRIVATE nav_dir      && Навигационный указатель
PRIVATE is_fdd       && Дискета ли?

PRIVATE st_nead, st_free, st_hole, f_flt, f_count, f_list, mss
PRIVATE drv_1, w, f_out, i
f_flt = 0
f_count = ADIR(f_list, f_from+".A*")

*
*  А дискета ли?! Это очччень важно!
*
drv_1  = LEFT(f_to,2)
is_fdd = INLIST(drv_1, "A:", "B:")

nav_dir = "NEXT"

DO WHILE .T.
  
  DO CASE

  CASE nav_dir = "NEXT"
    f_flt = f_flt+1
    IF f_flt > f_count
      EXIT
    ENDIF
    IF is_fdd
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  [ Вставили очередную дискету ][ Прекращаем запись! ]"
      mss(3) = ""
      IF Out_Mess(5, "mss") = 2
        EXIT
      ENDIF
    ENDIF
    nav_dir = "FDD_TEST"

  CASE nav_dir = "FDD_TEST"
    *------------------------------------------------------------------------
    *      Проверяем, что с дискетой...
    *
    nav_dir = "HOLE_SPACE"
    IF is_fdd
      w = drv_1
      CALL IsDisk WITH w
      IF w # drv_1
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"А есть ли на Вашем компьютере драйв "+drv_1+" и установлена ли на нем дискета?"
        mss(3) = CHR(0)+"   И как поступим дальше?[ Вставляем дискету ][ Отказываемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "FDD_TEST"
        CASE w = 2
          EXIT
        ENDCASE
      ENDIF
    ENDIF

  CASE nav_dir = "HOLE_SPACE"
    *------------------------------------------------------------------------
    *      А достаточно ли велик диск вообще?...
    *
    nav_dir = "FREE_SPACE"
    STORE 0 TO st_free, st_hole
    DO Disk_Space WITH drv_1, st_free, st_hole
    IF f_list(f_flt,2) > st_hole
      IF is_fdd
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"На драйве "+drv_1+" ВООБЩЕ нет столько места, сколько требуется!"
        mss(3) = CHR(0)+"  И как поступим дальше?[ Сменим дискету ][ Откажемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "FDD_TEST"
        CASE w = 2
          EXIT
        ENDCASE
      ELSE
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"На драйве "+drv_1+" ВООБЩЕ нет столько места, сколько требуется!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        EXIT
      ENDIF
    ENDIF

  CASE nav_dir = "FREE_SPACE"
    *------------------------------------------------------------------------
    *      А достаточно ли свободного места на диске?...
    *
    nav_dir = "COPY!"
    DO Disk_Space WITH drv_1, st_free, st_hole
    IF f_list(f_flt,2) > st_free
      IF .NOT. is_fdd
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"На драйве "+drv_1+" нет свободного места, сколько требуется!"
        mss(3) = CHR(0)+"   Как поступим дальше?[ Удалим кое-что... ][ Сменим дискету ][ Откажемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "CLEAR"
        CASE w = 2
          nav_dir = "FDD_TEST"
        CASE w = 3
          EXIT
        ENDCASE
      ELSE
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"На драйве "+drv_1+" нет свободного места, сколько требуется!"
        mss(3) = CHR(0)+"   Как поступим дальше?[ Удалим кое-что... ][ Откажемся ]"
        mss(4) = ""
        w = Out_Mess(7, "mss")
        DO CASE
        CASE w = 1
          nav_dir = "CLEAR"
        CASE w = 2
          EXIT
        ENDCASE
      ENDIF
    ENDIF

  CASE nav_dir = "CLEAR"
    *------------------------------------------------------------------------
    *      Очищаем...
    *
    pth_w = LEFT(f_to, RAT("\", f_to))
    
    i = Fdd_Dir(pth_w, f_list(f_flt,2))
    DO WHILE .NOT. EMPTY(i)
      DELETE FILE (i)
      i = Fdd_Dir(pth_w, f_list(f_flt,2))
    ENDDO
    nav_dir = "FREE_SPACE"

  CASE nav_dir = "COPY!"
    *------------------------------------------------------------------------
    *      Копируем!
    *
    nav_dir = "NEXT"
    f_out = f_to+RIGHT(f_list(f_flt,1),4)
    
    IF File_O(f_out)
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Файл "+f_out+" уже есть!"
      mss(3) = CHR(0)+"  Как с ним поступим? [ Обновим ] [ Оставим ]"
      mss(4) = ""
      IF Out_Mess(7, "mss") = 2
        LOOP
      ENDIF
      DELETE FILE (f_out)
    ENDIF
    
    DO Wt_Mess WITH "Пишем "+f_out
    pth_w = LEFT(f_from, RAT("\", f_from))
    COPY FILE (pth_w+f_list(f_flt,1)) TO (f_out)
    DO Wt_Mess
  OTHERWISE
    EXIT
  ENDCASE

ENDDO
*--------------------------------------------------------------------------

RETURN
