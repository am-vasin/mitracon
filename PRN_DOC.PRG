*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� prn_doc      ��о�｀砒┴ ��むォ ��瓱�           01.02.97 21:09:08 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                蹍������ ������キ�� � �ョ�皋 ぎ�祠キ��.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Prn_Doc
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy

SELECT 0
USE PATTERN
SET FILTER TO EMPTY(DATE_OFF)
GO TOP
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� ��゛���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Prnt OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Prnt OF (menu_name) Do Sw_Mode WITH "Prnt"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("DOC_PROMPT")

DO D_Wins WITH ln, wd, "��゛��� ぎ�祠キ皰�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS DOC_PROMPT:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Prnt")
    ENDIF

  CASE what_do = "Prnt"    && ������キ┘ ���ォ � �ョ�碎 ぎ�祠キ��

    DO One_Doc
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           ��ぃ�皰→� ��゛��.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 01.02.97 21:29:17 夕
PROCEDURE Prp_Tabs

PRIVATE pg_flt, line_flt, txt_h, i, j, fields_q, h_lines

txt_h   = PATTERN
pg_flt  = 1
lin_flt = 0
h_lines = 0
fields_q = 0

IF LEN(txt_h) = 0
  RETURN 0
ENDIF

DO WHILE LEN(txt_h) > 0
  i = AT(CHR(13)+CHR(10), txt_h)
  j = AT(CHR(12), txt_h)
  DO CASE

  CASE i = 0 .AND. j = 0              && ��甄イ�闖 痰牀�� ��甄イ�ォ 痰����肓
    h_lines = h_lines+1
    DIMENSION txt(h_lines, 2)
    txt(h_lines, 1) = txt_h
    txt(h_lines, 2) = pg_flt
    txt_h     = ""
    DO Seek_F                         && �薀� ���� � 筮爼�珮ガ ����皰→�

  CASE j = 0 .OR. i < j .AND. i # 0   && �腑爛き�� 痰牀�� �� 痰����罐
    h_lines = h_lines+1
    DIMENSION txt(h_lines, 2)
    txt(h_lines, 1) = LEFT(txt_h, i-1)
    txt(h_lines, 2) = pg_flt
    txt_h     = SUBSTR(txt_h, i+2)
    DO Seek_F                         && �薀� ���� � 筮爼�珮ガ ����皰→�
    lin_flt   = lin_flt+1
    
  CASE i = 0 .OR. j < i .AND. j # 0   && ��甄イ�闖 痰牀�� �� 痰����罐
    h_lines = h_lines+1
    DIMENSION txt(h_lines, 2)
    txt(h_lines, 1) = LEFT(txt_h, j-1)
    txt(h_lines, 2) = pg_flt
    txt_h     = SUBSTR(txt_h, j+1)
    DO Seek_F                         && �薀� ���� � 筮爼�珮ガ ����皰→�
    lin_flt   = 0
    pg_flt    = pg_flt+1
  OTHERWISE
*    SUSP &&&&&&&&&&&&&&&&
  ENDCASE
ENDDO
  
RETURN pg_flt

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          ｡��｀皖� ぎ�祠キ��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 01.02.97 22:00:51 夕
PROCEDURE One_Doc

PRIVATE ext_fl, ext_ff, c_vis_f
PRIVATE txt, flds, n_pgs, i, j, n_d, p_fl, l_fl, h_txt, st
DIMENSION txt(1,2), flds(1, 4)

ext_fl    = 0     && ����, ※え��� � 痰���
ext_ff    = 0     && ����, ※え��� ↓├� ��゛�肓
c_vis_f   = "*"   && �━〓� - �爬Л�� ������ ���� ↓├� ��゛�肓
txt(1, 1) = ""    && ��ぅ爨━�� 痰牀��
txt(1, 2) = 1     && ���ム 痰����肓

flds(1, 1) = -1   && ���ム 痰����肓
flds(1, 2) =  0   && ���ム 痰牀�� �� 痰����罐
flds(1, 3) =  0   && ���ム 痰��≧�
flds(1, 4) = " "  && ��ぅ爨━�� ����
n_pgs = Prp_Tabs()

IF n_pgs = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��゛�� �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Fil_Bl   && ������錺� ゛���
IF Do_Print()
  SELECT 0
  USE TXT_DOCS ORDER TAG DOC_NUM DESCENDING
  IF .NOT. SEEK(STR(DOC_CODE,5))
    n_d = 1
  ELSE
    IF YEAR(DOC_DATE) # YEAR(DATE())
      n_d = 1
    ELSE
      n_d = DOC_NUM+1
    ENDIF
  ENDIF
  APPEND BLANK
  REPLACE DOC_CODE WITH PATTERN.DOC_CODE, ;
          DOC_NUM  WITH n_d,              ;
          DOC_DATE WITH DATE(),           ;
          DOC_TIME WITH TIME(),           ;
          PAGE_LEN WITH PATTERN.PAGE_LEN, ;
          FONT     WITH PATTERN.FONT,     ;
          F_FEED   WITH PATTERN.F_FEED
  IF ext_fl # 0     && ����, ※え��� � 痰���
    REPLACE VISUAL_L WITH flds(ext_fl, 4)
  ENDIF
  IF ext_ff # 0     && ����, ※え��� ↓├� ��゛�肓
    REPLACE VISUAL_F WITH flds(ext_ff, 4)
  ENDIF
  h_txt = ""
  p_fl  = 0
  l_fl  = 0
  FOR i = 1 TO ALEN(txt,1)
    IF p_fl # txt(i, 2)
      p_fl = p_fl+1
      l_fl = 0
      h_txt = h_txt+IIF(p_fl = 1, "", CHR(12))
    ELSE
      h_txt = h_txt+CHR(13)+CHR(10)
    ENDIF
    st = txt(i, 1)
    FOR j = 1 TO ALEN(flds,1)
      IF flds(j, 1) = p_fl .AND. flds(j, 2) = l_fl
        st = STUFF(st, flds(j, 3)+1, LEN(flds(j, 4)), flds(j, 4))
      ENDIF
    ENDFOR
    h_txt = h_txt+st
    l_fl = l_fl+1
  ENDFOR
  REPLACE D_TEXT WITH h_txt
  RELEASE h_txt
  DO P_Doc_T
  USE
  SELECT PATTERN

ENDIF  

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     蹍������ 筮爼�牀����� ���ォ.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 01.02.97 22:41:14 夕
PROCEDURE Seek_F
PRIVATE j, i0, i1, s, s0

j = ALEN(txt,1)
s = txt(j, 1)

i0 = AT("[", s)
DO WHILE i0 # 0
  i1 = AT("]", s)
  s = STUFF(s, i0, 1, " ")
  IF i1 # 0
    s = STUFF(s, i1, 1, " ")
  ENDIF
  IF i0 < i1
    fields_q = fields_q+1
    DIMENSION flds(fields_q, 4)
    flds(fields_q, 1) = pg_flt
    flds(fields_q, 2) = lin_flt
    flds(fields_q, 3) = i0-1
    flds(fields_q, 4) = SPACE(i1-i0+1)
    s0 = SUBSTR(s, i0, i1-i0+1)
    IF .NOT. EMPTY(s0)    && ��゛�� 腑�-皰 ������キ - ※え��� ����!
      IF c_vis_f $ s0
        ext_ff = fields_q     && ����щ��ガ ィ� ↓├� ��゛�肓!
      ELSE
        ext_fl = fields_q     && ����щ��ガ ィ� � 痰牀��!
      ENDIF
    ENDIF
  ENDIF
  i0 = AT("[", s)
ENDDO

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍������ ������キ�� ゛����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 01.02.97 23:28:21 夕
PROCEDURE Fil_Bl

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

PRIVATE w_page, l_page, f_page, s, ex, i, j, c_sav

w_page = IIF(FONT = " ", 80, 120)   && ��爬�� 痰����肓
l_page = PAGE_LEN
f_page = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MIN(l_page, WROWS("")-7), MIN(w_page-4, WCOLS("")-2), ALLTRIM(DOC_PROMPT)
@ 1, 1 CLEAR
DO Sun_Bord WITH 1, 1, WROWS()-2, WCOLS()-2
DEFINE WINDOW W_Ext FROM WLROW()+2, WLCOL()+2 TO WLROW()+WROWS()-3, WLCOL()+WCOLS()-3 NONE COLOR SCHEME 13
DEFINE WINDOW W_Int FROM 0, 0 TO l_page+1, w_page+1 SHADOW COLOR SCHEME 13
ACTIVATE WINDOW W_Ext
ACTIVATE WINDOW W_Int IN WINDOW W_Ext

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
c_sav = SET("CURSOR") = "ON"
DO WHILE .T.
  MOVE WINDOW W_Int TO -1, -1
  DO Show_Pos
  s = PADL("�矗. "+ALLTRIM(STR(f_page)), 7)
  ACTIVATE WINDOW (win_name) SAME
  @ 0, WCOLS()-8 SAY s COLOR SCHEME 5
  ACTIVATE WINDOW W_Int SAME
  SET CURSOR OFF
  CLEAR
  j = 0
  FOR i = 1 TO ALEN(txt,1)
    IF txt(i, 2) = f_page
      @ j, 0 SAY txt(i, 1)
      j = j+1
    ENDIF
  ENDFOR

  FOR i = 1 TO ALEN(flds,1)
    IF flds(i, 1) = f_page
      SET CURSOR ON
      @ flds(i, 2), flds(i, 3) GET flds(i, 4) WHEN Show_F()
    ENDIF
  ENDFOR
  ex = 0

  ON KEY LABEL PgDn   DO Exit WITH 1
  ON KEY LABEL PgUp   DO Exit WITH -1
  ON KEY LABEL Escape DO Exit WITH 0
  ON KEY LABEL F1 DO Move_W WITH  0, -1
  ON KEY LABEL F3 DO Move_W WITH  0,  1
  ON KEY LABEL F2 DO Move_W WITH -1,  0
  ON KEY LABEL F4 DO Move_W WITH  1,  0
  READ CYCLE
  ON KEY LABEL PgDn
  ON KEY LABEL PgUp
  ON KEY LABEL Escape
  ON KEY LABEL F1
  ON KEY LABEL F3
  ON KEY LABEL F2
  ON KEY LABEL F4
  DO CASE
  CASE ex = -1
    f_page = MAX(f_page-1, 1)
  CASE ex = 1
    f_page = MIN(f_page+1, n_pgs)
  OTHERWISE
    EXIT
  ENDCASE
  
ENDDO

IF c_sav
  SET CURSOR ON
ELSE
  SET CURSOR OFF
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW W_Ext
RELEASE WINDOW W_Int
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�            蹍������ - �÷�｀砒┴ �諷��� ├ 蹣������ 筮爼�.            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 02.02.97 00:08:32 夕
PROCEDURE Exit
PARAMETER v

ex = v
KEYBOARD CHR(23)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          ����Ε�┘ 痰����肓.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 02.02.97 00:28:05 夕
PROCEDURE Show_Pos
PRIVATE s, s1, s2, s3, s4, i

s  = "����Ε�┘ 痰����肓: "
s1 = ALLTRIM(STR(-WLCOL("W_Int")+WLCOL("W_Ext")-1))
s  = s+PADR(" "+s1+" F1", 10)
s1 = ALLTRIM(STR(-WLROW("W_Int")+WLROW("W_Ext")-1))
s  = s+PADR(" "+s1+" F2", 10)

s1 = ALLTRIM(STR(WCOLS("W_Int")-WCOLS("W_Ext")+WLCOL("W_Int")-WLCOL("W_Ext")+1))
s  = s+PADR(" "+s1+" F3", 10)
s1 = ALLTRIM(STR(WROWS("W_Int")-WROWS("W_Ext")+WLROW("W_Int")-WLROW("W_Ext")+1))
s  = s+PADR(" "+s1+" F4", 10)

ACTIVATE WINDOW (win_name) SAME
@ WROWS()-1, 3 SAY s
ACTIVATE WINDOW W_Int SAME
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       �ムガラキ┘ ��痰� � ����.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 02.02.97 00:41:15 夕
PROCEDURE Move_W
PARAMETERS l, c

PRIVATE lw, cw

lw = MAX(WLROW("W_Ext")+WROWS("W_Ext")-WLROW("W_Int")-WROWS("W_Int")-2, l)
lw = MIN(WLROW("W_Ext")-WLROW("W_Int"), lw)

cw = MAX(WLCOL("W_Ext")+WCOLS("W_Ext")-WLCOL("W_Int")-WCOLS("W_Int")-2, c)
cw = MIN(WLCOL("W_Ext")-WLCOL("W_Int"), cw)

MOVE WINDOW W_Int BY lw, cw
DO Show_Pos

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �ョ���ガ ぎ�祠キ�?                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 03.02.97 22:04:55 夕
PROCEDURE Do_Print

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

PRIVATE ex
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 48, "�ョ���碎 ぎ�祠キ�?"

*             �ョ���碎 ぎ�祠キ�?
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                            �
*�   < �ョ���碎 Ctrl+W > < �皖���碎瘴 Esc >   �
*�                                            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 5 GET ex PICTURE "@*HT \! �ョ���碎 Ctrl+W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       �謔�瓷│�ガ ���� ��珮��...                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 23.02.97 12:38:29 夕
PROCEDURE Show_F

PRIVATE l_, c_, i, delta_l, delta_c, l_new, c_new

i  = VARREAD()
i  = SUBSTR(i, 6)
i  = VAL(LEFT(i, LEN(i)-3))

l_ = flds(i, 2)+WLROW("W_Int")-WLROW("W_Ext")+1
c_ = flds(i, 3)+WLCOL("W_Int")-WLCOL("W_Ext")+1

STORE 0 TO delta_l, delta_c

IF l_ < 0 .OR. l_ > WROWS("W_Ext")-1   && �� √�え� �� ▲珥┴���
  l_new = FLOOR(WROWS("W_Ext")/2)      && 鴜�ガ瘴 �襪※�竄赱� �� 瓮爛え��
  delta_l = l_new-l_
ENDIF
IF c_ < 0 .OR. c_+LEN(flds(i,4))-1 > WCOLS("W_Ext")-1   && �� √�え� �� ��爬М�����
  c_new = FLOOR((WCOLS("W_Ext")-LEN(flds(i,4)))/2)      && 鴜�ガ瘴 �襪※�竄赱� �� 瓮爛え��
  c_new = MAX(c_new, 0)
  delta_c = c_new-c_
ENDIF
IF delta_l # 0 .OR. delta_c # 0
   DO Move_W WITH delta_l, delta_c
ENDIF

RETURN .T.
