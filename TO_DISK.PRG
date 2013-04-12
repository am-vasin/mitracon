*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� ����஢���� ᯨ᪠ 䠩���.                   �
*�             ( ��।������ ���筨� - 㪠�뢠� �㤠 )                 �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE To_Disk
PARAMETER file_list, def_pth && ���ᨢ � ������� 䠩��� � ���� ��� ��� 䠩��
                             &&   �� 㬮�砭�� (��᫥���� ����易⥫쭮)

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

PRIVATE ex, nm_w, nm_1, drv_1, mss, w, c_sav
PRIVATE nav_dir      && ������樮��� 㪠��⥫�
PRIVATE is_fdd       && ��᪥� ��?
PRIVATE mode         && ���᮪ ��� 䠩�
PRIVATE dst_name     && ��� 䠩��, � ����� ���� ����஢����

                     &&   ���塞 � ������塞 ���� ������
ex   = 1             &&
nm_w = IIF(TYPE("def_pth") = "C", PADR(def_pth,40), SPACE(40) )
mode = IIF(TYPE("file_list(1)") = "C", "list", "file")

*------------------------------------------------------------------------

*           �㤠 ��襬 �����?
*������������������������������������������������Ŀ
*� �� ������ ����� ���� � ��� 䠩�� ���������Ŀ �
*� �  ....:....1....:....2....:....3....:....4  � �
*� ���������������������������������������������� �
*�        < OK Ctrl-W > < �⪠������ Esc >        �
*��������������������������������������������������

*
*     ��।��塞 ᪮�쪮 �ॡ���� ����.
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

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 52, "�㤠 ��襬 �����?"

DO Sun_Bord WITH 2, 3, 4, 48
@ 2, 5 SAY " ������ ����� ���� "+IIF(mode = "file", "� ��� 䠩�� ","")

c_sav = SET("CURSOR") = "ON"
nav_dir = "INPUT"
DO WHILE .T.
  DO CASE

  CASE nav_dir = "INPUT"
  SET CURSOR ON
    *-------------------------------------------------------------------
    *      ���� ����� ������
    *
    SHOW WINDOW (win_name)
    ACTIVATE WINDOW (win_name)
    @ 3,  6 GET nm_w
    @ 5, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

    READ CYCLE
    HIDE WINDOW (win_name)
    ACTIVATE SCREEN
    SET CURSOR OFF
    IF ex = 2
      nav_dir = "EXIT"    && �뫥⠥�
      LOOP
    ENDIF

    *
    *  "����뢠��" �, �� �����...
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
        mss(2) = CHR(0)+"�� ⠪ � �� 㪠���� ��� 䠩��!"
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
    *   ��।��塞 �ࠩ� - � ����� �ਣ������...
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
    *  � ��᪥� ��?! �� ���祭� �����!
    *
    is_fdd = INLIST(drv_1, "A:", "B:")
    
    nav_dir = "FDD_TEST"

  CASE nav_dir = "FDD_TEST"
    *------------------------------------------------------------------------
    *      �஢��塞, �� � ��᪥⮩...
    *
    nav_dir = "TRUE_PATH"
    IF is_fdd
      w = drv_1
      CALL IsDisk WITH w
      IF w # drv_1
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"� ���� �� �� ��襬 �������� �ࠩ� "+drv_1+" � ��⠭������ �� �� ��� ��᪥�?"
        mss(3) = CHR(0)+"   � ��� ����㯨� �����?[ ��⠢�塞 ��᪥�� ][ ���塞 ���� ][ �⪠�뢠���� ]"
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
    *      �஢��塞, ���४⭮��� ���...
    *
    w = LEFT(nm_1, LEN(nm_1)-1)
    nav_dir = "HOLE_SPACE"
    IF LEN(w) > 0 .AND. (LEN(w) # 2 .OR. RIGHT(w,1) # ":")
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� 㪠���� �����४�� ����!"
        mss(3) = CHR(0)+"  �� ������?[ ���塞 ���� ][ �⪠�뢠���� ]"
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
    *      � �����筮 �� ����� ��� �����?...
    *
    nav_dir = "FREE_SPACE"
    DO Disk_Space WITH drv_1, st_free, st_hole
    IF st_nead > st_hole
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� �ࠩ�� "+drv_1+" ������ ��� �⮫쪮 ����, ᪮�쪮 �ॡ����!"
        mss(3) = CHR(0)+"  � ��� ����㯨� �����?[ ������ ���� ][ �⪠����� ]"
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
    *      � �����筮 �� ᢮������� ���� �� ��᪥?...
    *
    nav_dir = "COPY!"
    IF st_nead > st_free
      IF .NOT. Is_Dir(w)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� �ࠩ�� "+drv_1+" ��� ᢮������� ����, ᪮�쪮 �ॡ����!"
        mss(3) = CHR(0)+"   ��� ����㯨� �����?[ ������ ���-��... ][ ������ ���� ][ �⪠����� ]"
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
    *      ��頥�...
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
    *      �����㥬!
    *
    nav_dir = "EXIT"
    PRIVATE j, sour, dest
    
    dest = FULLPATH(nm_1)
    IF mode = "list"
      FOR i = 1 TO f_qnt
        j = MAX(RAT("\",file_list(i)), RAT(":",file_list(i)))+1
        j = SUBSTR(file_list(i),j)

        sour = FULLPATH(file_list(i))
        IF dest+UPPER(j) = sour   && ����⪠ ����஢��� 䠩� �� ���� ��
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"���� "+j+" �� ᪮��஢��!"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          LOOP
        ENDIF

        IF File_O(nm_1+j)
          DIMENSION mss(4)
          mss(1) = ""
          mss(2) = CHR(0)+"���� "+j+" 㦥 ����!"
          mss(3) = CHR(0)+"  ��� � ��� ����㯨�? [ ������� ] [ ��⠢�� ]"
          mss(4) = ""
          IF Out_Mess(7, "mss") = 2
            LOOP
          ENDIF
          DELETE FILE (nm_1+j)
        ENDIF

        DO Wt_Mess WITH "��襬 "+j
        COPY FILE (file_list(i)) TO (nm_1+j)
        DO Wt_Mess
      ENDFOR
      ret_p = nm_1
    ELSE
      j = MAX(RAT("\",file_list), RAT(":",file_list))+1
      j = SUBSTR(file_list,j)

      sour = FULLPATH(file_list)
      IF dest+UPPER(j) = sour+UPPER(dst_name)  && ����⪠ ����஢��� 䠩� �� ���� ��
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"���� "+dst_name+" �� ����ᠭ!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        EXIT
      ENDIF

      IF File_O(nm_1+dst_name)
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"���� "+nm_1+dst_name+" 㦥 ����!"
        mss(3) = CHR(0)+"  ��� � ��� ����㯨�? [ ������� ] [ ��⠢�� ]"
        mss(4) = ""
        IF Out_Mess(7, "mss") = 2
          ret_p = nm_1
          EXIT
        ENDIF
        DELETE FILE (nm_1+dst_name)
      ENDIF

      DO Wt_Mess WITH "��襬 "+nm_1+dst_name
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