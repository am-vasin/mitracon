*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� New_Info     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �祗�罔����讚�� ��Л�腑�┘.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.04.2004 様�
PROCEDURE New_Info
PARAMETER c_code

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

PRIVATE ex, tmpFunc, tmp_seg
PRIVATE sel_sav
sel_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ALIAS STO_4404 AGAIN ORDER TAG CODE
SEEK c_code
SELECT 0
USE (path_comm+"SUBJECT") ALIAS SJ_4404 AGAIN ORDER TAG STO_CODE
tmpFunc = PADR("�祗�罔����讚�� ��Л�腑�┘ �� ������", FSIZE("FUNCTION", "SJ_4404"))
tmp_seg = SPACE(FSIZE("SEGMENT", "SJ_4404"))
IF SEEK(c_code)
  IF .NOT. EMPTY(FUNCTION)
    tmpFunc = FUNCTION
  ENDIF
  tmp_seg = SEGMENT
ENDIF

                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�         ��Ж罔� 臼臼 臼臼憶臼越臼臼憶臼越臼臼� 臼臼憶臼          �
*�                                                                  �
*� ....:....!....:....!....:....!....:....!....:....!....:....!.... �
*�                                                                  �
*�                          �ィ�キ� ....:.                          �
*�                              < OK >                              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 70, "�祗�罔����讚�� ��Л�腑�┘"

*      �〓� ���ォ ゛����
*
@  2, 11 SAY "��Ж罔�"
@  2, COL()+1 GET STO_4404.PREFIX
@  2, COL()+1 GET STO_4404.NAME
@  2, COL()+1 GET STO_4404.PRODUCER
@  4, 3 GET tmpFunc
@  6, 28 SAY "�ィ�キ�" GET tmp_seg
CLEAR GETS
@  7, 32 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

USE IN STO_4404
USE IN SJ_4404
SELECT (sel_sav)

RETURN
