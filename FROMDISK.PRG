*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ ����牀����� 甎�瓷� ������.                   �
*�           ( ｯ爛ぅ�キ�覃 爛с�赳�� - 礫�щ��� �皖磴� )                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE FromDisk
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
PRIVATE st_nead, st_free, st_hole, f_qnt, i, j, pth_w, ret_p
IF mode = "list"
  f_qnt = ALEN(file_list)
  DIMENSION file_list(f_qnt)
ENDIF
ret_p = ""

STORE 0 TO st_free, st_hole

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 52, "�ぅ �薀� ����襯?"

DO Sun_Bord WITH 2, 3, 4, 48
@ 2, 5 SAY " ���Θ皀 ����覃 �竄� "

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
    nav_dir = "EXIST?"
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
      mss(2) = CHR(0)+"� 礫������� ���� え爛�皰爬� �� 紜���モ ������:"
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
    *      ����珮ガ!
    *
    nav_dir = "EXIT"
    PRIVATE f_f, f_t

    IF mode = "list"
      FOR i = 1 TO f_qnt
        IF File_O(file_list(i))
          DIMENSION mss(4)
          mss(1) = ""
          mss(2) = CHR(0)+"���� "+F_Name(file_list(i))+" 礒� メ碎!"
          mss(3) = CHR(0)+"  ��� � �━ ��痰祚━? [ ｡��※� ] [ �痰�※� ]"
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
          mss(2) = CHR(0)+"��� ����� "+F_Name(file_list(i))+" �モ �メ��!"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          EXIT
        ENDIF

        DO Wt_Mess WITH "��茱� "+F_Name(file_list(i))
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
        mss(2) = CHR(0)+"���� "+F_Name(file_list)+" 礒� メ碎!"
        mss(3) = CHR(0)+"  ��� � �━ ��痰祚━? [ ｡��※� ] [ �痰�※� ]"
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
        mss(2) = CHR(0)+"��� ����� "+F_Name(file_list)+" �モ �メ��!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        EXIT
      ENDIF

      DO Wt_Mess WITH "��茱� "+F_Name(file_list)
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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�      蹍������ �襪カキ�� ━キ� ����� ├ ������ ( ┼� �ク����� )       �
*�                             甎ユ�筥��罔�.                             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE F_Name
PARAMETERS full_sp

PRIVATE i000

i000 = MAX(RAT("\", full_sp), RAT(":", full_sp))
IF i000 = LEN(full_sp)
  RETURN ""
ENDIF
RETURN SUBSTR(full_sp,i000+1)
