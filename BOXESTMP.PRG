*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� BoxesTmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ���甌� ��牀｀� か� �����き��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.09.2000 様�
PROCEDURE BoxesTmp
PARAMETERS start_no
PRIVATE s_sav

s_sav = SELECT()

IF .NOT. USED("BOXNO")   && �爛�キ���� ����� ラ� �モ: 甌Г�碎!
  SELECT 0
  USE (path_as+"MATERS")
  SELECT 0
  USE (path_as+"BOXNO")
  SELECT 0
  DELETE FILE (tmpo_path+"BOXESTMP.DBF")
  CREATE DBF (tmpo_path+"BOXESTMP.DBF") (REF_NO N(10))
  SET RELATION TO REF_NO INTO BOXNO
ELSE
  SELECT BOXESTMP
ENDIF
IF RECCOUNT() = 0
  DO Modi_Box WITH .T.
ENDIF
IF RECCOUNT() = 0
  SELECT (s_sav)
  RETURN
ENDIF

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
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
SELECT 0
DO Use_Dummy
SELECT BOXESTMP
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Create OF (menu_name) PROMPT "��Г�碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Create OF (menu_name) DO Sw_Mode WITH "Create"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-18   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 25

DO D_Wins WITH ln, wd, "���甌� ��牀｀�", 0, 0
@ 1, 2 SAY "��牀！� N陳陳��爲�牀→�"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins DO Sw_Mode WITH "Create"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ;
           BOXID   = ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO)):12, ;
           BOXMARK = IIF(EMPTY(BOXNO.MASK),  ;
                     ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO)), ;
                     BOXNO.MASK):12,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Choose", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    EXIT

  CASE what_do = "Modify"    && ��玻オ罔� ��牀！�

    DO Modi_Box
    what_do = "List"

  CASE what_do = "Create"    && ��Г��┘ ��牀！�

    DO Modi_Box WITH .T.
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
*   �モ! � ������ 甄竍�� �� ���琺��ガ!
*
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_Box     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��玻オ罔�/甌Г��┘ ��牀！�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2000 陳�
PROCEDURE Modi_Box
PARAMETERS new

PRIVATE new_n
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
* ��牀！� OOOOOOOO9999
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*� ��爲�牀→� 9999999999  �メ 999.9 �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpWeight, tmpMask
ex = 1
IF new
  tmpWeight = 0
  tmpMask   = "          "
ELSE
  tmpWeight = BOXNO.WEIGHT
  tmpMask   = BOXNO.MASK
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, IIF(new, "����� ��牀！�", "��牀！� "+ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO)))

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  3 SAY "��爲�牀→�" GET tmpMask
@  3, 26 SAY "�メ" GET tmpWeight PICTUR "999.9"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF new
    PRIVATE tmpCus, tmpSup, tmpCusO, tmpSupO
    IF "ACT_S_T.DBF" $ UPPER(DBF("SOURCE_T"))   && 踸ガ�覃 ���
      tmpCus = SOURCE_T.SUP_CODE
      tmpSup = sup_own
    ELSE
      tmpCus = SOURCE_T.CUS_CODE
      tmpSup = SOURCE_T.SUP_CODE
    ENDIF
    SELECT 0
    STORE "" TO tmpCusO, tmpSupO
    USE (path_as+"OFFICE") ORDER TAG CODE
    IF SEEK(tmpCus)
      tmpCusO = NAME
    ENDIF
    IF SEEK(tmpSup)
      tmpSupO = NAME
    ENDIF
    USE
    SELECT MATERS
    new_n = BOX_MT
    REPLACE BOX_MT WITH BOX_MT+1
    FLUSH
    SELECT BOXNO
    APPEND BLANK
    REPLACE BOXNO     WITH new_n,         ;
            OFFICE    WITH MATERS.OFFICE, ;
            BOX_YEAR  WITH YEAR(DATE()),  ;
            DATE      WITH DATE(),        ;
            DATE_MOVE WITH DATE(),        ;
            MOVEST    WITH "0",           ;
            NAKL_TYPE WITH "I",           ;
            NAKL_NUM  WITH IIF(TYPE("SOURCE_T.DOC_NUM") = "N", STR(SOURCE_T.DOC_NUM, 10), SOURCE_T.DOC_NUM),  ;
            NAKL_DATE WITH SOURCE_T.DOC_DATE, ;
            SUP_CODE  WITH tmpSup,        ;
            CUS_CODE  WITH tmpCus,        ;
            SUP_OFFIC WITH tmpSupO,       ;
            CUS_OFFIC WITH tmpCusO,       ;
            TECHNID   WITH SOURCE_T.TECHNID,;
            TECHNTP   WITH SOURCE_T.TECHNTP
    new_n = RECNO()
    SELECT BOXESTMP
    APPEND BLANK
    REPLACE REF_NO WITH new_n
  ENDIF
  SELECT BOXNO
  REPLACE WEIGHT WITH tmpWeight, MASK WITH tmpMask
  SELECT BOXESTMP
ENDIF
SELECT BOXESTMP
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
