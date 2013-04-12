*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                     ���४�� �ଠ� ���㬥�⮢.                     �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Corr_Doc

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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "���४��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Parms OF (menu_name) PROMPT "��ਡ���" KEY Ctrl-A, "F1"
ON SELECTION PAD Parms OF (menu_name) Do Sw_Mode WITH "Parms"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

USE DOC_FORM

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, f_name, cl
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("DOC_PROMPT")+12

DO D_Wins WITH ln, wd, "���㬥���", 2, 0
what_do = "list"
@ 1, 2 SAY SPACE(FSIZE("DOC_PROMPT"))+            " ��� ��� ���"
@ 2, 2 SAY PADR("���㬥��",(FSIZE("DOC_PROMPT")))+" � � � � � �"
cl = SCHEME(13,1)+","+SCHEME(13,2)+",,,,,"+SCHEME(14,2)

f_name = ""
DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    Do Sw_Mode WITH "Parms"
*
*    BROWSE - ����
*
    BROWSE FIELDS DOC_PROMPT:W=.F.:H="",     ;
           DOC_T0 = IIF(EMPTY(DOC_H),  " ", "�"):H="":W=Sav_F("Doc_T", DOC_HF),   ;
           DOC_F0 = IIF(EMPTY(DOC_F),  " ", "�"):H="":W=Sav_F("Doc_F", DOC_FF),   ;
           PAG_T0 = IIF(EMPTY(PAGE_H), " ", "�"):H="":W=Sav_F("Pag_T", PAGE_HF),  ;
           PAG_F0 = IIF(EMPTY(PAGE_F), " ", "�"):H="":W=Sav_F("Pag_F", PAGE_FF),  ;
           Det_00 = IIF(EMPTY(DETAIL), " ", "�"):H="":W=Sav_F("Det_0", DETAILF),  ;
           Det_01 = IIF(EMPTY(DETAIL1), " ", "�"):H="":W=Sav_F("Det_1", DETAIL1F) ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") COLOR (cl)
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ���४�� ���㬥��

    DO Corr WITH f_name
    what_do = "list"

  CASE what_do = "Parms"    && ���४�� ��ਡ�⮢

    DO C_Parms
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                         ���࠭塞 ��� ����!                           �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Sav_F
PARAMETER fact_name, sw

f_name = fact_name

RETURN sw

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                         ���४�� ���㬥��.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Corr
PARAMETER part

PRIVATE tit, mss, ex, ps_list, psd_list, ps_num, ps_wd, i, wk_f, enab

SET MEMOWIDTH TO 200
ps_num = MEMLINES(DICTION)
ps_wd  = 0
enab = .F.
IF ps_num # 0
  DIMENSION ps_list(ps_num)
  FOR i = 1 TO ps_num
    ps_list(i) = MLINE(DICTION, i)
    ps_wd = MAX(ps_wd, LEN(ps_list(i)))
  ENDFOR
  FOR i = 1 TO ps_num
    ps_list(i) = PADR(ps_list(i), ps_wd)
  ENDFOR
ENDIF

ps_num = MEMLINES(DIC_DET)
ps_wd  = 0
IF ps_num # 0
  DIMENSION psd_list(ps_num)
  FOR i = 1 TO ps_num
    psd_list(i) = MLINE(DIC_DET, i)
    ps_wd = MAX(ps_wd, LEN(psd_list(i)))
  ENDFOR
  FOR i = 1 TO ps_num
    psd_list(i) = PADR(psd_list(i), ps_wd)
  ENDFOR
ENDIF

DO CASE
CASE part = "Doc_T"
  tit = "��������� ���㬥��"
  wk_f = DOC_H
  enab = DOC_HF
CASE part = "Doc_F"
  tit = "����砭�� ���㬥��"
  wk_f = DOC_F
  enab = DOC_FF
CASE part = "Pag_T"
  tit = "��������� ��࠭���"
  wk_f = PAGE_H
  enab = PAGE_HF
CASE part = "Pag_F"
  tit = "����砭�� ��࠭���"
  wk_f = PAGE_F
  enab = PAGE_FF
CASE part = "Det_0"
  tit = "��⠫쭠� ��ப�"
  wk_f = DETAIL
  enab = DETAILF
CASE part = "Det_1"
  tit = "��⠫쭠� ��ப�"
  wk_f = DETAIL1
  enab = DETAIL1F
ENDCASE

IF EMPTY(wk_f)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+tit+" � ������ ��砥 ���������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

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

PRIVATE ln, wd, sav  &&
                     &&
ln = WROWS("")-4     &&
wd = WCOLS("")-4     &&
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, tit
SCROLL WROWS()-2, 0, WROWS()-1, WCOLS()-1, 1
*------------------------------------------------------------------------
*      ���� ����� ������
*
@ WROWS()-1, FLOOR((WCOLS()-32)/2) SAY "OK - Ctrl/W;  �⪠������ - Esc"
DEFINE WINDOW W_Ext FROM WLROW()+2, WLCOL()+2 TO WLROW()+WROWS()-3, WLCOL()+WCOLS()-3 ;
         NONE COLOR SCHEME 13

DEFINE WINDOW W_Int FROM -1, -2 TO WROWS()-1, WCOLS()-3 ;
       COLOR SCHEME 13

ACTIVATE WINDOW W_Ext

SET RESOURCE ON
sav = .T.

PUSH MENU _MSYSMENU
SET SYSMENU TO

SET SYSMENU AUTOMATIC

DEFINE PAD _Edit OF _MSYSMENU PROMPT "������஢���" COLOR SCHEME 4
ON PAD _Edit OF _MSYSMENU ACTIVATE POPUP _Edit
DEFINE POPUP _Edit MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR _MED_UNDO  OF _Edit PROMPT "�⬥��"     KEY Ctrl+Z, "Ctrl-Z"
DEFINE BAR _MED_CUT   OF _Edit PROMPT "��१���"   KEY Ctrl+X, "Ctrl-X"
DEFINE BAR _MED_COPY  OF _Edit PROMPT "����஢���" KEY Ctrl+C, "Ctrl-C"
DEFINE BAR _MED_PASTE OF _Edit PROMPT "�⠢���"    KEY Ctrl+V, "Ctrl-V"

DEFINE PAD _Tools OF _MSYSMENU PROMPT "�����㬥��" COLOR SCHEME 4
ON PAD _Tools OF _MSYSMENU ACTIVATE POPUP _Tools
DEFINE POPUP _Tools MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF _Tools PROMPT "�ᥢ������   F1"  KEY F1
DEFINE BAR 2 OF _Tools PROMPT "����ᨬ����  F2" KEY F2
ON SELECTION BAR 1 OF _Tools DO View_Sub
ON SELECTION BAR 2 OF _Tools DO Spec

DEFINE PAD Save OF _MSYSMENU PROMPT "���࠭��� Ctrl-W" COLOR SCHEME 3
ON SELECTION PAD Save OF _MSYSMENU DO Ex_Modify WITH .T.

DEFINE PAD Escape OF _MSYSMENU PROMPT "�⪠������ Esc" COLOR SCHEME 3
ON SELECTION PAD Escape OF _MSYSMENU DO Ex_Modify

ON KEY LABEL Esc DO Ex_Modify
ON KEY LABEL Ctrl+W DO Ex_Modify WITH .T.
ON KEY LABEL F1 DO View_Sub
ON KEY LABEL F2 DO Spec
ON KEY LABEL F10 ACTIVATE MENU _MSYSMENU

DO CASE
CASE part = "Doc_T"
  MODIFY MEMO DOC_H WINDOW W_Int IN WINDOW W_Ext
  IF .NOT. sav
    REPLACE DOC_H WITH wk_f
  ENDIF
CASE part = "Doc_F"
  MODIFY MEMO DOC_F WINDOW W_Int IN WINDOW W_Ext
  IF .NOT. sav
    REPLACE DOC_F WITH wk_f
  ENDIF
CASE part = "Pag_T"
  MODIFY MEMO PAGE_H WINDOW W_Int IN WINDOW W_Ext
  IF .NOT. sav
    REPLACE PAGE_H WITH wk_f
  ENDIF
CASE part = "Pag_F"
  MODIFY MEMO PAGE_F WINDOW W_Int IN WINDOW W_Ext
  IF .NOT. sav
    REPLACE PAGE_F WITH wk_f
  ENDIF
CASE part = "Det_0"
  MODIFY MEMO DETAIL WINDOW W_Int IN WINDOW W_Ext
  IF .NOT. sav
    REPLACE DETAIL WITH wk_f
  ENDIF
CASE part = "Det_1"
  MODIFY MEMO DETAIL1 WINDOW W_Int IN WINDOW W_Ext
  IF .NOT. sav
    REPLACE DETAIL1 WITH wk_f
  ENDIF
ENDCASE
ON KEY LABEL Esc
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL Ctrl+W
ON KEY LABEL F10

SET RESOURCE OFF
RELEASE WINDOW W_Int
RELEASE WINDOW W_Ext

SET SYSMENU OFF
POP MENU _MSYSMENU
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                          ��ࠡ��稪 Esc.                              �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Ex_Modify
PARAMETER toSave

ON KEY LABEL F1 DO View_Sub

RELEASE WINDOW W_Ext
DEACTIVATE MENU _MSYSMENU
sav = toSave

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                       ��⠭���� ��ࠡ��稪�.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Set_F1

ON KEY LABEL F1 DO View_Sub

RETURN .T.

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                         ���⪠ ��ࠡ��稪�.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                        ���祭� �ᥢ�������.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE View_Sub
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
PRIVATE ln, wd, i, s, i1, i2, ps_num, tmp_arr, ps_wd

ps_wd = 6
DO CASE
CASE INLIST(part, "Det_0", "Det_1")
  IF TYPE("psd_list") = "U"
    RETURN
  ENDIF
  ps_num = ALEN(psd_list)
  DIMENSION tmp_arr(ps_num)
  FOR i = 1 TO ps_num
    tmp_arr(i) = psd_list(i)
    IF ps_wd < LEN(tmp_arr(i))
      ps_wd = LEN(tmp_arr(i))
    ENDIF
  ENDFOR
OTHERWISE
  IF TYPE("ps_list") = "U"
    RETURN
  ENDIF
  ps_num = ALEN(ps_list)
  IF ps_num <= 0
    RETURN
  ENDIF
  DIMENSION tmp_arr(ps_num)
  FOR i = 1 TO ps_num
    tmp_arr(i) = ps_list(i)
    IF ps_wd < LEN(tmp_arr(i))
      ps_wd = LEN(tmp_arr(i))
    ENDIF
  ENDFOR
ENDCASE

ln = MIN(ps_num+3, WROWS("")-7)
wd = ps_wd+4

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "�ᥢ������"

*------------------------------------------------------------------------
*      ���� ����� ������
*
i = 1
DO WHILE .T.
  i = MAX(i, 1)
  i = MIN(i, ps_num)
  @ 1, 1 MENU tmp_arr, ps_num, ln-3
  READ MENU TO i
  s = READKEY() % 256
  IF s = 12
    EXIT
  ENDIF
  IF s = 15
    i1 = AT("{", tmp_arr(i))
    i2 = AT("}", tmp_arr(i))
    IF i1 > 0 .AND. i2 > 0 .AND. i2 > i1
      KEYBOARD "{LbRace}"+SUBSTR(tmp_arr(i), i1+1, i2-i1-1)+"{RbRace}"
    ENDIF
    EXIT
  ENDIF
ENDDO

POP KEY
RELEASE WINDOW (win_name)
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                       ��ਡ��� ���㬥��.                             �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE C_Parms

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

PRIVATE ex, p_l, l_s, n_c, fnt, ff0, ff1, p_d, dir_l, dir_n, s_sav, d_name
                     &&   ���塞 � ������塞 ���� ������

dir_n = 0
IF File_O(base_path+"PRN_DIRS.DBF")
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"PRN_DIRS")
  SCAN
    dir_n = dir_n+1
    DIMENSION dir_l(dir_n)
    dir_l(dir_n) = NAME
  ENDSCAN
  USE
  SELECT (s_sav)
ENDIF
IF dir_n = 0
  dir_n = 1
  DIMENSION dir_l(dir_n)
  dir_l(dir_n) = SPACE(32)
ENDIF

ex   = 1
p_l = PAGE_LEN
l_s = LEFT_FIELD
n_c = N_COPIES
fnt = IIF(EMPTY(FONT), 1, 2)
ornt = IIF(EMPTY(ORIENT), 1, 2)
ff0 = IIF(F_FEED < 0, 1, 2)
ff1 = MAX(F_FEED, 0)
p_d = MAX(P_DIR,1)
d_name = DOC_PROMPT

*------------------------------------------------------------------------

*
*���������������������������������������������Ŀ
*�  ....:....!....:....!....:....!....:....!   �
*�       ����� ��࠭��� (� ��ப��) 999        �
*�        ����� ᫥�� (� ᨬ�����) 99         �
*�               ��᫮ ����� 99                �
*� �� ���� ��������������������������������Ŀ �
*� �      ( ) ��ଠ���     ( ) �����      � �
*� ������������������������������������������� �
*� �� �ਥ���� ���������������������������Ŀ �
*� �      ( ) �������        ( ) ��졮����   � �
*� ������������������������������������������� �
*� �� �ண�� � ���� ���㬥�� �������������Ŀ �
*� � ( ) ��ଠ�  ( ) ��ப  ��᫮ ��ப 999 � �
*� ������������������������������������������� �
*�             ���ࠢ����� ����              �
*�     ��������������������������������Ŀ      �
*�     �....:....!....:....!....:....!..�      �
*�     ����������������������������������      �
*�      < OK Ctrl-W > < �⪠������ Esc >       �
*�����������������������������������������������
PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 21, 49, "��騥 ��ࠬ����"
DO Sun_Bord WITH 6, 3, 8, 45
@ 6, 5 SAY " ���� "
DO Sun_Bord WITH 9, 3, 11, 45
@ 9, 5 SAY " �ਥ���� "
DO Sun_Bord WITH 12, 3, 14, 45
@ 12, 5 SAY " �ண�� � ���� ���㬥�� "
@ 15,15 SAY "���ࠢ����� ����"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2,  4 GET d_name
@ 3,  9 SAY "����� ��࠭��� (� ��ப��)" GET p_l PICTURE "@Z 999" VALID T_Fld(1)
@ 4, 10 SAY "����� ᫥�� (� ᨬ�����)" GET l_s PICTURE "@Z 99" VALID T_Fld(2)
@ 5, 17 SAY "��᫮ �����" GET n_c PICTURE "@Z 99" VALID T_Fld(3)
@ 7, 10 GET fnt PICTURE "@*RH ��ଠ���    ;�����"
@ 10, 10 GET ornt PICTURE "@*RH �������      ;��졮����"
@ 13, 5 GET ff0 PICTURE "@*RH ��ଠ� ;��ப" VALID T_Fld(4)
@ 13, 29 SAY "��᫮ ��ப" GET ff1 PICTURE "@Z 999" WHEN ff0 = 2  VALID T_Fld(5)
@ 16, 8 GET p_d PICTURE "@^" FROM dir_l SIZE 1, 32 WHEN dir_n > 1
@ 19, 8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Sav_R()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�              ���� �ணࠬ�� �஢�ન ��室� �� ����.                 �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE T_Fld
PARAMETERS f_num

PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

DO CASE
CASE f_num = 1     && ����� ��࠭���
  IF p_l < 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����� ���⪠� ��࠭��? ��㬠����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF p_l > 255
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����� ��࠭��� �� ����� ���� ����� 255"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE f_num = 2     && ����� ᫥��
  IF l_s < 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����쪮, ᪮�쪮?!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE f_num = 3     && ���-�� �����
  IF n_c < 1
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����쪮, ᪮�쪮?!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE f_num = 4     && ��� �ண���
  IF ff0 = 1
    ff1 = 0
    SHOW GET ff1
  ENDIF
CASE f_num = 5     && ��᫮ ��ப �ண���
  IF ff1 < 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����쪮, ᪮�쪮?!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDCASE

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 �ணࠬ�� ��࠭���� ���������.                       �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Sav_R

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

REPLACE PAGE_LEN   WITH p_l,   ;
        LEFT_FIELD WITH l_s,   ;
        N_COPIES   WITH n_c,   ;
        FONT       WITH IIF(fnt = 1, " ", "1"), ;
        ORIENT     WITH IIF(ornt = 1, " ", "1"), ;
        P_DIR      WITH p_d,   ;
        DOC_PROMPT WITH d_name, ;
        F_FEED     WITH IIF(ff0 = 1, -1, ff1)

RETURN .T.

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                            ��� ᯥ�ᨬ�����.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Spec

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

*�����������������������������Ŀ
*� � � � � � � � � � � � � � � �
*�                             �
*� � � � � � � � � � � � � � � �
*�                             �
*� � � � � � � � � � � � � � � �
*�������������������������������

PRIVATE line1, line2, line3, point1, point2, point3
line1 = "�¿��˻��Ѹ�ҷ"
line2 = "�Ŵ��ι��ص�׶"
line3 = "�����ʼ��Ͼ�н"
STORE 0 TO point1, point2, point3
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 33, "�ᥢ����䨪�"
@ 2, 3 SAY "� � � � � � � � � � � � � �"
@ 4, 3 SAY "� � � � � � � � � � � � � �"
@ 6, 3 SAY "� � � � � � � � � � � � � �"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2, 3 GET point1 PICTURE "@*IHT ;;;;;;;;;;;;;"  SIZE 1,1,1
@ 4, 3 GET point2 PICTURE "@*IHT ;;;;;;;;;;;;;"  SIZE 1,1,1
@ 6, 3 GET point3 PICTURE "@*IHT ;;;;;;;;;;;;;"  SIZE 1,1,1
READ CYCLE

DO CASE
CASE point1 # 0
  _CLIPTEXT = SUBSTR(line1, point1, 1)
CASE point2 # 0
  _CLIPTEXT = SUBSTR(line2, point2, 1)
CASE point3 # 0
  _CLIPTEXT = SUBSTR(line3, point3, 1)
ENDCASE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN