*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� LettView     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       蹍甃�矗 皀�痰� 甌�♂キ��                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.2005 様�
PROCEDURE LettView
PARAMETERS tmpID		&& ��� 甌�♂キ��

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�          �ガ� ....:....!....:....!....:....!..            �
*楽� �オ痰 甌�♂キ�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖                                                         崖
*崖                                                         崖
*崖                                                         崖
*崖                                                         崖
*崖                                                         崖
*崖                                                         崖
*崖                                                         崖
*崖                                                         崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*� ��Г��� ....:....!....:....!....:....!.. ��.��.���� 99:99 �
*�                         < OK Esc >                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, s_sav, tmpWho, tmpText, tmpTime
m.ex = 1
m.s_sav = SELECT()
SELECT 0
USE base_path+"PERSONS" SHARED AGAIN ORDER TAG CODE ALIAS PRS_5A22
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ORDER TAG MESS_ID ALIAS MSS_5A22
SEEK m.tmpID
= SEEK(MSS_5A22.WHO, "PRS_5A22")
m.tmpWho  = ALLTRIM(PRS_5A22.FAMILY)+" "+LEFT(PRS_5A22.NAME, 1)+" "+LEFT(PRS_5A22.S_NAME, 1)
m.tmpWho  = PADR(m.tmpWho, 32)
m.tmpText = MSS_5A22.MESS_TEXT
m.tmpTime = LEFT(MSS_5A22.CR_TIME, 5)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 16, 63, "蹍甃�矗 甌�♂キ��"
DO Sun_Bord WITH  3,  2, 12, 60, " �オ痰 甌�♂キ�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2, 12 SAY "�ガ�" GET MSS_5A22.TITLE WHEN .F.
@  4,  3 EDIT m.tmpText NOMODIFY SIZE 8, 57 COLOR (SCHEME(1, 1))
@ WROWS()-3, 3 SAY "��Г���" GET m.tmpWho WHEN .F.
@ WROWS()-3, COL()+1 GET MSS_5A22.CR_DATE WHEN .F.
@ WROWS()-3, COL()+1 GET m.tmpTime WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-5) GET m.ex PICTURE "@*HT \? OK Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

USE IN MSS_5A22
USE IN PRS_5A22

SELECT (m.s_sav)

RETURN
