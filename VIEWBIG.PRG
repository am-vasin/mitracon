*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� ViewBig      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      蹍甃�矗 �爬絎き諷 �爐ム��.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.09.2000 様�
PROCEDURE ViewBig
PARAMETER pMax

IF pMax = 0
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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

PRIVATE c_name, show
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
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS C_0920 AGAIN
SEEK BIGORD.CUS_CODE
c_name = CUS_NAME
USE
show = .F.
SELECT BIGORD
SCAN
  IF DAY_SUM > pMax
    show = .T.
    EXIT
  ENDIF
ENDSCAN
IF .NOT. show
  RETURN
ENDIF
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*

*敖陳陳����陳陳陳�� ぎ�祠キ矣陳陳陳�� ぅ�貭陳朕
*�! ��.��.���� 999999999999.99 999999999999.99�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ln, wd
ln = MIN(RECCOUNT(), 10)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 44

DO D_Wins WITH ln, wd, "踸絎� ����膈諷 ぅ�ィ �� き鍖", 1, 0
@ 1, 1 SAY LEFT(c_name, WCOLS()-2)
@ 2, 2 SAY "陳陳�����陳陳陳�� ぎ�祠キ矣陳陳陳�� ぅ��"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS BIG = IIF(DAY_SUM > pMax, "�", " "):1:H="", ;
                  DOC_DATE:H="":V=ReCalc(),         ;
                  DOC_SUM:H="":P="999999999999.99", ;
                  DAY_SUM:H="":P="999999999999.99"  ;
           FREEZE DOC_DATE     ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

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
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ReCalc       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �ムメ腑� Л�腑�┤ ��゛�肓.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.09.2000 陳�
PROCEDURE ReCalc

PRIVATE r_sav, s000
r_sav = RECNO()
SELECT BIGORD
SCAN
  GO BIGORD.DOC_REF IN BIGDOC
  SELECT BIGDOC
  REPLACE DOC_DATE WITH BIGORD.DOC_DATE, ENTER_DAT WITH BIGORD.DOC_DATE
  s000 = Cus_Sum(BIGDOC.CUS_CODE, BIGDOC.DOC_DATE, BIGDOC.FIRM)
  SELECT BIGORD
  REPLACE DAY_SUM WITH s000
ENDSCAN

GO r_sav

RETURN .T.
