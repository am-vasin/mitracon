*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Seek_C       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ��ぁ�� 痰���� ├ ����                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.12.2012 様�
PROCEDURE Seek_C
PARAMETERS prmName 

PRIVATE tmpName, tmpShab, fltExpr, rSav, mss

m.tmpShab = ""
m.fltExpr = ""
m.tmpName = SYS(15, m.lwr, ALLTRIM(m.prmName))
DIMENSION mss[3]
mss[1] = ""
mss[2] = CHR(0)+"Hモ �� �き��� ��ゅ�わ薀�� ��Б����"
mss[3] = ""

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

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"OKSM") SHARED AGAIN ORDER TAG B_NAME ALIAS CH_OKSM
SET FILTER TO m.fltExpr $ SYS(15, m.lwr, B_NAME) .OR. EMPTY(m.fltExpr)

PRIVATE i, rc, retV
m.rc = RECNO()
FOR m.i = 1 TO LEN(m.tmpName)
  IF .NOT. SEEK(LEFT(m.tmpName, m.i))
    EXIT
  ENDIF
  m.rc = RECNO()
ENDFOR
GO m.rc
m.retV = ""

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("B_NAME")+FSIZE("CODE")+1

DO D_Wins WITH m.ln, m.wd, ALLTRIM(m.prmName), 1, 0
@ 1, 2 SAY "�┼赳�"
@ 2, 2 SAY "��つ�矗���"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Filter"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CODE:H="",     ;
                  B_NAME:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF .NOT. EOF() .AND. .NOT. BOF()
      m.retV = CODE
    ENDIF
    EXIT

  CASE m.what_do = "Filter"    && �痰���→� 筥�赳��

    m.tmpName = F_Shab(m.tmpShab)

    IF TYPE("m.tmpName") = "C"
      m.fltExpr = SYS(15, m.lwr, m.tmpName)
      m.rSav = RECNO()
      IF .NOT. m.fltExpr $ SYS(15, m.lwr, B_NAME) .AND. .NOT. EMPTY(m.fltExpr)
        GO TOP
        IF EOF()
          DO Out_Mess WITH 7, "mss"
          m.tmpName = m.tmpShab
          m.fltExpr = SYS(15, m.lwr, m.tmpShab)
          GO m.rSav
        ENDIF
      ENDIF
      m.tmpShab = m.tmpName
      ACTIVATE WINDOW (win_name+"_M") SAME
      @ 1, 9 SAY PADR(m.tmpShab, 40)
    ENDIF
   
    m.what_do = "List"

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
SELECT (m.s_sav)

RETURN m.retV

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� F_Shab       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �〓� ��゛��� か� 筥�赳��                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 28.12.2012 陳�
PROCEDURE F_Shab
PARAMETERS sValue

PRIVATE rValue

m.rValue = .F.
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� ....:....!....:....!....:....!....:....! �
*�                                          �
*�     < OK Ctrl-W > < �皖���碎瘴 Esc >     �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpSab
m.ex = 1
m.tmpShab = ""
IF TYPE("m.sValue") = "C"
  m.tmpShab = ALLTRIM(m.sValue)
ENDIF
m.tmpShab = PADR(m.tmpShab, 40)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 46, "��゛�� か� 筥�赳��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 3 GET m.tmpShab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  m.rValue = ALLTRIM(m.tmpShab)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.rValue
