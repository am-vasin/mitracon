*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Copy_A       ��о�｀砒┴ ��むォ ��瓱�           18.03.97 22:14:57 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     蹍������ ����牀����� �璢│�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Copy_A
PARAMETER f_from, f_to  && �皖磴�-�磴�: �竄� � ━キ� ������ (．� ��瘉�爛�┤)

PRIVATE nav_dir      && ��※��罔���覃 礫���皀��
PRIVATE is_fdd       && ��瓷モ� ��?

PRIVATE st_nead, st_free, st_hole, f_flt, f_count, f_list, mss
PRIVATE drv_1, w, f_out, i
f_flt = 0
f_count = ADIR(f_list, f_from+".A*")

*
*  � え瓷モ� ��?! �皰 �艸腑�� ��Ν�!
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
      mss(2) = CHR(0)+"  [ �痰�※�� �腑爛き竡 え瓷モ� ][ 踳�����ガ ����瘡! ]"
      mss(3) = ""
      IF Out_Mess(5, "mss") = 2
        EXIT
      ENDIF
    ENDIF
    nav_dir = "FDD_TEST"

  CASE nav_dir = "FDD_TEST"
    *------------------------------------------------------------------------
    *      蹍▲瑙ガ, 艪� � え瓷モ��...
    *
    nav_dir = "HOLE_SPACE"
    IF is_fdd
      w = drv_1
      CALL IsDisk WITH w
      IF w # drv_1
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"� メ碎 �� �� ��茱� ����跪皀爛 む��� "+drv_1+" � 竅����←キ� �� �� �ガ え瓷モ�?"
        mss(3) = CHR(0)+"   � ��� ��痰祚━ ���跏�?[ �痰�←錺� え瓷モ� ][ �皖�щ��ガ瘴 ]"
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
    *      � ぎ痰�皰膈� �� ▲�┴ え瓷 〓�♂�?...
    *
    nav_dir = "FREE_SPACE"
    STORE 0 TO st_free, st_hole
    DO Disk_Space WITH drv_1, st_free, st_hole
    IF f_list(f_flt,2) > st_hole
      IF is_fdd
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� む��▲ "+drv_1+" ������ �モ 痰��讓� �メ��, 瓷��讓� 矗ァ礇矚�!"
        mss(3) = CHR(0)+"  � ��� ��痰祚━ ���跏�?[ ��キ━ え瓷モ� ][ �皖�Ε�瘴 ]"
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
        mss(2) = CHR(0)+"�� む��▲ "+drv_1+" ������ �モ 痰��讓� �メ��, 瓷��讓� 矗ァ礇矚�!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        EXIT
      ENDIF
    ENDIF

  CASE nav_dir = "FREE_SPACE"
    *------------------------------------------------------------------------
    *      � ぎ痰�皰膈� �� 瓣�｀き��� �メ�� �� え瓷�?...
    *
    nav_dir = "COPY!"
    DO Disk_Space WITH drv_1, st_free, st_hole
    IF f_list(f_flt,2) > st_free
      IF .NOT. is_fdd
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� む��▲ "+drv_1+" �モ 瓣�｀き��� �メ��, 瓷��讓� 矗ァ礇矚�!"
        mss(3) = CHR(0)+"   ��� ��痰祚━ ���跏�?[ ����━ ���-艪�... ][ ��キ━ え瓷モ� ][ �皖�Ε�瘴 ]"
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
        mss(2) = CHR(0)+"�� む��▲ "+drv_1+" �モ 瓣�｀き��� �メ��, 瓷��讓� 矗ァ礇矚�!"
        mss(3) = CHR(0)+"   ��� ��痰祚━ ���跏�?[ ����━ ���-艪�... ][ �皖�Ε�瘴 ]"
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
    *      �腮��ガ...
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
    *      ����珮ガ!
    *
    nav_dir = "NEXT"
    f_out = f_to+RIGHT(f_list(f_flt,1),4)
    
    IF File_O(f_out)
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"���� "+f_out+" 礒� メ碎!"
      mss(3) = CHR(0)+"  ��� � �━ ��痰祚━? [ ｡��※� ] [ �痰�※� ]"
      mss(4) = ""
      IF Out_Mess(7, "mss") = 2
        LOOP
      ENDIF
      DELETE FILE (f_out)
    ENDIF
    
    DO Wt_Mess WITH "��茱� "+f_out
    pth_w = LEFT(f_from, RAT("\", f_from))
    COPY FILE (pth_w+f_list(f_flt,1)) TO (f_out)
    DO Wt_Mess
  OTHERWISE
    EXIT
  ENDCASE

ENDDO
*--------------------------------------------------------------------------

RETURN
