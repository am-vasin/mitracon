*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Banks        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ��玻オ罔� 甎�瓷� ������.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01/12/1999 様�
PROCEDURE Banks

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FR_9113 AGAIN
SELECT 0
USE (base_path+"BANKS") ORDER TAG ALL_NAMES ALIAS B_9113 AGAIN
SET RELATION TO FIRM_CODE INTO FR_9113

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "��玻オ罔�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "����←." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "����./��瘁�." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*������� ��Б��┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����キ陳陳�
*�99999999999999999999999999999999999999999999999999 99.99.9999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*���牀皖�� ��Б��┘ 999999999999999  ��爼� 999 9999999999999999�
*���瘍.瘍モ 99999999999999999999 ��玻.瘍モ 99999999999999999999�
*�                        ��� 999999999                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 61, 59)

DO D_Wins WITH ln, wd, "���甌� ������", 0, 3
@ 1, 2 SAY "������ ��Б��┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����キ"
@ WROWS()-4,  2 SAY "��牀皖�� ��Б��┘                  ��爼�"
@ WROWS()-3,  2 SAY "��瘍.瘍モ                      ��玻.瘍モ"
@ WROWS()-2, 26 SAY "���"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Del_Bank
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS L_NAME:50:H="",    ;
                  DATE_OFF:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ��玻オ罔� �����

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Modi_Line
    ENDIF
    what_do = "List"

  CASE what_do = "Del"     && ����キ┘/〓瘁����←キ┘

    DO Del_Bank
    what_do = "List"

  CASE what_do = "Add"     && ����←キ┘ 筥爼�

    DO Modi_Line WITH .T.
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
USE
SELECT FR_9113
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot
PRIVATE cl
cl = SCHEME(13,6)
ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-4, 20 SAY B_NAME COLOR (cl)
@ WROWS()-4, 43 SAY FR_9113.FIRM_CODE COLOR (cl)
@ WROWS()-4, 47 SAY FR_9113.B_NAME COLOR (cl)
@ WROWS()-3, 12 SAY ACC_NO COLOR (cl)
@ WROWS()-3, 43 SAY CORR_NO COLOR (cl)
@ WROWS()-2, 30 SAY BIK COLOR (cl)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_Line    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ����←キ┘/��玻オ罔� �爛く爬閧��.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/12/1999 陳�
PROCEDURE Modi_Line
PARAMETERS new

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
IF .NOT. EMPTY(DATE_OFF) .AND. .NOT. new
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��爼� 磴��キ�, ����� 甃諱� ゥ ��玻オ皋牀��碎?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽� ���� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖9999999999999999999999999999999999999999999999999999999999999999999999崖
*崖                                                                      崖
*崖  ��牀皖�� ��Б��┘ 9999999999999999  ��爼� 999 9999999999999999(F1)  崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*� ��瘍モ�覃 瘍モ 99999999999999999999    ��玻. 瘍モ 99999999999999999999 �
*�                                                                        �
*�                             ��� 999999999                              �
*�                                                                        �
*�                    < OK Ctrl-W > < �皖���碎瘴 Esc >                    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, cd, n0, n1, f_cd, f_n, c1, c2, b0
IF new
  cd  = "   "
  n0  = SPACE(16)
  n1  = SPACE(70)
  f_cd= "   "
  f_n = SPACE(16)
  c1  = SPACE(16)
  c2  = SPACE(16)
  b0  = SPACE(9)
ELSE
  cd  = BANK_CODE
  n0  = B_NAME
  n1  = L_NAME
  f_cd= FIRM_CODE
  f_n = FR_9113.B_NAME
  c1  = ACC_NO
  c2  = CORR_NO
  b0  = BIK
ENDIF

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 76, IIF(new, "����←キ┘ ", "��玻オ罔� ")+"�����"
DO Sun_Bord WITH  2,  2,  6, 73, " ���� "
@ 5, 67 SAY "(F1)"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  3 GET n1
@  5,  5 SAY "��牀皖�� ��Б��┘" GET n0
@  5, 41 SAY "��爼�" GET f_cd WHEN new .AND. Set_F1() VALID Tst_Firm()
@  5, 51 GET f_n WHEN .F.
@  7,  3 SAY "��瘍モ�覃 瘍モ" GET c1
@  7, 42 SAY "��玻. 瘍モ" GET c2
@  9, 31 SAY "���" GET b0
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF new
    SET ORDER TO TAG BANK DESCENDING
    IF .NOT. SEEK(f_cd)
      cd = "001"
    ELSE
      cd = RIGHT(STR(1000+VAL(BANK_CODE)+1),3)
    ENDIF
    APPEND BLANK
    SET ORDER TO TAG ALL_NAMES
  ENDIF
  REPLACE BANK_CODE WITH cd,  ;
          B_NAME    WITH n0,  ;
          L_NAME    WITH n1,  ;
          FIRM_CODE WITH f_cd,;
          ACC_NO    WITH c1,  ;
          CORR_NO   WITH c2,  ;
          BIK       WITH b0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ��玻オ皚�痰� ������キ�� ゛����.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/12/1999 陳�
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(n1)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ ━� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",2)
  RETURN .F.
ENDIF

IF EMPTY(n0)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��牀皖�� ━� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",3)
  RETURN .F.
ENDIF

IF EMPTY(f_cd)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 甌▲琥キ�� �ギ≦�え�� 礫���碎 筥爼�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",4)
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Del_Bank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     ����キ┘/〓瘁����←キ┘ �����.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/12/1999 陳�
PROCEDURE Del_Bank

IF BETWEEN(RECNO(), 1, RECCOUNT())
  IF EMPTY(DATE_OFF)
    REPLACE DATE_OFF WITH DATE()
  ELSE
    REPLACE DATE_OFF WITH {}
  ENDIF
ENDIF
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→� �÷�｀砒┴� F1.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.01.1999 陳�
PROCEDURE Set_F1

ON KEY LABEL F1 DO What_F

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Firm     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �諷�� ├ ���� ���� 筥爼�.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.01.1999 陳�
PROCEDURE Tst_Firm

IF ex = 2 .OR. EMPTY(f_cd) .OR. READKEY() % 256 = 12
  ON KEY LABEL F1
  RETURN .T.
ENDIF

f_cd = CHRTRAN(f_cd, shab1, shab2)
SHOW GET f_cd
IF .NOT. SEEK(f_cd, "FR_9113")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 筥爼� 礫���� �アム��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
f_n = FR_9113.B_NAME
SHOW GET f_n
IF .NOT. EMPTY(FR_9113.DATE_OFF)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 筥爼� 磴��キ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

ON KEY LABEL F1

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� What_F       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �襦�� 筥爼� ├ 甎�瓷�.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.01.1999 陳�
PROCEDURE What_F

DO Ch_Firm WITH f_cd, f_n

SHOW GET f_cd
SHOW GET f_n

RETURN
