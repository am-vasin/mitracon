*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ ����牀����� 甎�瓷� ������.                   �
*�             ( ｯ爛ぅ�キ�覃 �痰�膈┴ - 礫�щ��� �磴� )                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE To_Disk
PARAMETER file_list, def_pth && ��瘁│ � ━キ��� ������ � �竄� ┼� ━� �����
                             &&   �� 祠������� (��甄イ�┤ �ギ￥��皀�讚�)

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

PRIVATE ex, nm_w, nm_1, drv_1, mss, w, c_sav
PRIVATE nav_dir      && ��※��罔���覃 礫���皀��
PRIVATE is_fdd       && ��瓷モ� ��?
PRIVATE mode         && ���甌� ┼� ����
PRIVATE dst_name     && ━� �����, � ��皰琺� ┐モ ����牀���┘

                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
nm_w = IIF(TYPE("def_pth") = "C", PADR(def_pth,40), SPACE(40) )
mode = IIF(TYPE("file_list(1)") = "C", "list", "file")

*------------------------------------------------------------------------

*           �磴� ��茱� ����襯?
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ���Θ皀 ����覃 �竄� � ━� ����� 陳陳陳陳陳� �
*� �  ....:....1....:....2....:....3....:....4  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*
*     ｯ爛ぅ�錺� 瓷��讓� 矗ァ礇矚� �メ��.
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

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 52, "�磴� ��茱� ����襯?"

DO Sun_Bord WITH 2, 3, 4, 48
@ 2, 5 SAY " ���Θ皀 ����覃 �竄� "+IIF(mode = "file", "� ━� ����� ","")

c_sav = SET("CURSOR") = "ON"
nav_dir = "INPUT"
DO WHILE .T.
  DO CASE

  CASE nav_dir = "INPUT"
  SET CURSOR ON
    *-------------------------------------------------------------------
    *      �〓� ���ォ ゛����
    *
    SHOW WINDOW (win_name)
    ACTIVATE WINDOW (win_name)
    @ 3,  6 GET nm_w
    @ 5, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

    READ CYCLE
    HIDE WINDOW (win_name)
    ACTIVATE SCREEN
    SET CURSOR OFF
    IF ex = 2
      nav_dir = "EXIT"    && �覊モ�ガ
      LOOP
    ENDIF

    *
    *  "踸腑瘠��ガ" 皰, 艪� □カ�...
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
        mss(2) = CHR(0)+"�� ��� � �� 礫����� ━� �����!"
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
    *   ｯ爛ぅ�錺� む��� - � ΘЛ� �爬��え矚�...
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
    *  � え瓷モ� ��?! �皰 �艸腑�� ��Ν�!
    *
    is_fdd = INLIST(drv_1, "A:", "B:")
    
    nav_dir = "FDD_TEST"

  CASE nav_dir = "FDD_TEST"
    *------------------------------------------------------------------------
    *      蹍▲瑙ガ, 艪� � え瓷モ��...
    *
    nav_dir = "TRUE_PATH"
    IF is_fdd
      w = drv_1
      CALL IsDisk WITH w
      IF w # drv_1
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"� メ碎 �� �� ��茱� ����跪皀爛 む��� "+drv_1+" � 竅����←キ� �� �� �ガ え瓷モ�?"
        mss(3) = CHR(0)+"   � ��� ��痰祚━ ���跏�?[ �痰�←錺� え瓷モ� ][ �キ錺� �竄� ][ �皖�щ��ガ瘴 ]"
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
    *      蹍▲瑙ガ, ��玻オ皚�痰� �竄�...
    *
    w = LEFT(nm_1, LEN(nm_1)-1)
    nav_dir = "HOLE_SPACE"
    IF LEN(w) > 0 .AND. (LEN(w) # 2 .OR. RIGHT(w,1) # ":")
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� 礫����� �オ�玻オ皚覃 �竄�!"
        mss(3) = CHR(0)+"  �皰 ぅ��ガ?[ �キ錺� �竄� ][ �皖�щ��ガ瘴 ]"
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
    *      � ぎ痰�皰膈� �� ▲�┴ え瓷 〓�♂�?...
    *
    nav_dir = "FREE_SPACE"
    DO Disk_Space WITH drv_1, st_free, st_hole
    IF st_nead > st_hole
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� む��▲ "+drv_1+" ������ �モ 痰��讓� �メ��, 瓷��讓� 矗ァ礇矚�!"
        mss(3) = CHR(0)+"  � ��� ��痰祚━ ���跏�?[ ��キ━ �竄� ][ �皖�Ε�瘴 ]"
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
    *      � ぎ痰�皰膈� �� 瓣�｀き��� �メ�� �� え瓷�?...
    *
    nav_dir = "COPY!"
    IF st_nead > st_free
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� む��▲ "+drv_1+" �モ 瓣�｀き��� �メ��, 瓷��讓� 矗ァ礇矚�!"
        mss(3) = CHR(0)+"   ��� ��痰祚━ ���跏�?[ ����━ ���-艪�... ][ ��キ━ �竄� ][ �皖�Ε�瘴 ]"
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
    *      �腮��ガ...
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
    *      ����珮ガ!
    *
    nav_dir = "EXIT"
    PRIVATE j, sour, dest
    
    dest = FULLPATH(nm_1)
    IF mode = "list"
      FOR i = 1 TO f_qnt
        j = MAX(RAT("\",file_list(i)), RAT(":",file_list(i)))+1
        j = SUBSTR(file_list(i),j)

        sour = FULLPATH(file_list(i))
        IF dest+UPPER(j) = sour   && ���謔�� ����牀��碎 ���� �� �ィ� Ε
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"���� "+j+" �� 瓷���牀���!"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          LOOP
        ENDIF

        IF File_O(nm_1+j)
          DIMENSION mss(4)
          mss(1) = ""
          mss(2) = CHR(0)+"���� "+j+" 礒� メ碎!"
          mss(3) = CHR(0)+"  ��� � �━ ��痰祚━? [ ｡��※� ] [ �痰�※� ]"
          mss(4) = ""
          IF Out_Mess(7, "mss") = 2
            LOOP
          ENDIF
          DELETE FILE (nm_1+j)
        ENDIF

        DO Wt_Mess WITH "��茱� "+j
        COPY FILE (file_list(i)) TO (nm_1+j)
        DO Wt_Mess
      ENDFOR
      ret_p = nm_1
    ELSE
      j = MAX(RAT("\",file_list), RAT(":",file_list))+1
      j = SUBSTR(file_list,j)

      sour = FULLPATH(file_list)
      IF dest+UPPER(j) = sour+UPPER(dst_name)  && ���謔�� ����牀��碎 ���� �� �ィ� Ε
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"���� "+dst_name+" �� �������!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        EXIT
      ENDIF

      IF File_O(nm_1+dst_name)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"���� "+nm_1+dst_name+" 礒� メ碎!"
        mss(3) = CHR(0)+"  ��� � �━ ��痰祚━? [ ｡��※� ] [ �痰�※� ]"
        mss(4) = ""
        IF Out_Mess(7, "mss") = 2
          ret_p = nm_1
          EXIT
        ENDIF
        DELETE FILE (nm_1+dst_name)
      ENDIF

      DO Wt_Mess WITH "��茱� "+nm_1+dst_name
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
