*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ld_Note      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      踸�ョ��┘ � ��Ж罔� ������                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 21.04.2006 様�
PROCEDURE Ld_Note
PARAMETERS prmList, prmCode, prmROnly
*
* prmList  - ���ム 甎�瓷� (C),
* prmCode  - ��� ��Ж罔� (┼� NAME+PREFIX+PRODUCER か� �メ���め��� ��Ж罔�),
* prmROnly - 皰�讓� 艪キ┘.
*

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
PRIVATE s_sav, tmpNote, tmpName, tmpPref, tmpProd
m.s_sav = SELECT()
SELECT 0
IF TYPE("m.prmCode") = "N"
  USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_6421 ORDER TAG L_CODE
  IF .NOT. SEEK(m.prmList+STR(m.prmCode, 7))
    USE
    SELECT (m.s_sav)
    RETURN
  ENDIF
  m.tmpNote = NOTE

  USE (m.base_path+"STOCK") SHARED AGAIN ALIAS ST_6421 ORDER TAG CODE IN 0
  = SEEK(m.prmCode, "ST_6421")
  m.tmpName = ST_6421.NAME
  m.tmpPref = ST_6421.PREFIX
  m.tmpProd = ST_6421.PRODUCER
  USE IN ST_6421
ELSE
  USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_6421 ORDER TAG L_NAME
  IF .NOT. SEEK(m.prmList+m.prmCode)
    USE
    SELECT (m.s_sav)
    RETURN
  ENDIF
  m.tmpNote = NOTE

  m.tmpName = NAME
  m.tmpPref = PREFIX
  m.tmpProd = PRODUCER
ENDIF

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� PPPP ....:....!....:....!....:....!....: PPPPPPPP �
*楽� 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖                                                 崖
*崖                                                 崖
*崖                                                 崖
*崖                                                 崖
*崖                                                 崖
*崖                                                 崖
*崖                                                 崖
*崖                                                 崖
*崖                                                 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 16, 55, "踸�ョ��┘ � ��Ж罔� ������"
DO Sun_Bord WITH  3,  2, 13, 52, " 踸�ョ��┘ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
*@ 2, 4 SAY "��Ж罔�"
@ 2, 3 SAY m.tmpPref COLOR SCHEME 1
@ 2, COL()+1 SAY m.tmpName COLOR SCHEME 1
@ 2, COL()+1 SAY m.tmpProd COLOR SCHEME 1
IF m.prmROnly
  @ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \! OK "
  @ 4, 3 EDIT m.tmpNote SIZE 9, 48 COLOR (SCHEME(1,1)) NOMODIFY
ELSE
  @ 4, 3 EDIT m.tmpNote SIZE 9, 48 COLOR (SCHEME(1,1))
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
ENDIF

READ CYCLE

IF m.ex = 1 .AND. .NOT. m.prmROnly
  *
  * �矗���硅��ガ ゛���
  IF .NOT. NOTE == m.tmpNote
    REPLACE NOTE WITH m.tmpNote
  ENDIF
ENDIF
*--------------------------------------------------------------------------
USE
SELECT (m.s_sav)
POP KEY
RELEASE WINDOW (win_name)

RETURN
