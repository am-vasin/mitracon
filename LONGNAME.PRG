*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� LongName     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ��┃��� ━� か� ��┘���                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.02.2007 様�
PROCEDURE LongName
PARAMETERS prmCode

PRIVATE s_sav, tmpLong, mss, tmpName

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CUS_NAME") SHARED AGAIN ALIAS LN_70202 ORDER TAG CUS_CODE
IF .NOT. SEEK(m.prmCode)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� ��┘��� �� �爛ゃ甃�矗キ� か┃���� ━キ�"
  mss[3] = ""
  DO Out_Mess WITH 5, "mss"
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

m.tmpLong = ALLTRIM(CUS_NAME)
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS LN_70202 ORDER TAG CUS_CODE
SEEK m.prmCode
m.tmpName = CUS_NAME
USE
SELECT (m.s_sav)
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�    ��┘�� ....:....!....:....!....:....!....:....!....:....!    �
*楽� ������ ━� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�                             < OK >                              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 11, 69, "��┃��� ━� ��┘���"
DO Sun_Bord WITH  3,  2,  8, 66, " ������ ━� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 6 SAY "��┘��" GET m.tmpName WHEN .F.
@ 4, 3 EDIT m.tmpLong SIZE 4, 63 WHEN .F. COLOR (SCHEME(1, 1))
@ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \! OK "

READ CYCLE

*IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
