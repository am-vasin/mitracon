*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� SeekList     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ���瓷 甎�瓷� �� ���ム�                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.02.2006 様�
PROCEDURE SeekList

PRIVATE tmpNum, mss, s_sav, r_val

m.tmpNum = Get_List()
IF EMPTY(m.tmpNum)
  RETURN 0
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LT_6227
IF .NOT. SEEK(m.tmpNum)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"���甌� �� ���ぅ�..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (m.s_sav)
  RETURN 0
ENDIF

IF LT_6227.LST_ARM # m.arm
  DO L_Info
  USE
  SELECT (m.s_sav)
  RETURN 0
ENDIF

m.r_val = RECNO("LT_6227")
USE
SELECT (m.s_sav)

RETURN m.r_val

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� L_Info       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���♂キ┘ � 甎�瓷�                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.02.2006 陳�
PROCEDURE L_Info
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
PRIVATE p, a

USE (m.base_path+"ARM_LST") SHARED AGAIN ORDER TAG ARM_C ALIAS A_6227 IN 0
=SEEK(LT_6227.LST_ARM, "A_6227")
m.a = PADR(A_6227.ARM_N, 50)
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS A_6227 IN A_6227
=SEEK(LT_6227.WHO_CR, "A_6227")
m.p = ALLTRIM(A_6227.FAMILY)+" "+LEFT(A_6227.NAME, 1)+" "+LEFT(A_6227.S_NAME, 1)
m.p = PADR(m.p, 50)
USE IN A_6227

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� ��｀腑� �メ皰 ....:....!....:....!....:....!....:....!....:....!  �
*�                                                                   �
*� �キイΕ�      ....:....!....:....!....:....!....:....!....:....!  �
*�                          < OK >                                   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 71, "���甌� � "+ALLTRIM(STR(LT_6227.LST_NUM))
*DO Sun_Bord WITH  2,  2,  4, 49, " ������〓� ����� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 3 SAY "��｀腑� �メ皰" GET m.a
@ 4, 3 SAY "�キイΕ�     " GET m.p
CLEAR GETS

@ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \! OK  "

READ CYCLE

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
