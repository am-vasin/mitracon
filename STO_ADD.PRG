*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Sto_Add      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 �������皀�讚�� ┃筮爼�罔� � ��Ж罔�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.04.2004 様�
PROCEDURE Sto_Add
PARAMETER c_code, stop_corr

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

PRIVATE ex, tmpFunc, tmpSegment, tmpCorpC, tmpCorpN, segList, nList, i, swCorp
PRIVATE tmpPdf, tmpImage, tmpName
PRIVATE sel_sav
swCorp = .F.
sel_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ALIAS ST_4404 AGAIN ORDER TAG CODE
SEEK c_code
SELECT 0
USE (path_comm+"SUBJECT") ALIAS SUB_4404 AGAIN ORDER TAG SEGMENT
nList = 0
SCAN
  nList = nList+1
  DIMENSION segList(nList)
  segList(nList) = SEGMENT
ENDSCAN
IF nList = 0
  DIMENSION segList(1)
  segList(1) = SPACE(FSIZE("SEGMENT"))
  nList = 1
ENDIF
SET ORDER TO TAG STO_CODE
IF .NOT. SEEK(c_code)
  APPEND BLANK
  REPLACE STO_CODE WITH ST_4404.CODE,   ;
          PREFIX   WITH ST_4404.PREFIX, ;
          NAME     WITH ST_4404.NAME,   ;
          PRODUCER WITH ST_4404.PRODUCER
ENDIF
m.tmpFunc    = FUNCTION
m.tmpSegment = SEGMENT
m.tmpCorpC   = COR_CODE
m.tmpCorpN   = CORPUS
m.tmpPdf     = DATASHEET
m.tmpImage   = IMAGE
FOR i = 1 TO nList
  IF SYS(15, m.lwr, m.tmpSegment) = SYS(15, m.lwr, segList(i))
    m.tmpSegment = i
    EXIT
  ENDIF
ENDFOR
IF TYPE("tmpSegment") = "C"
  m.tmpSegment = 1
ENDIF

                     &&   ｡褊←錺� � ������錺� ���� ゛����
m.ex   = 1             &&

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*���Ж罔� 臼臼 臼臼憶臼越臼臼憶臼越臼臼憶臼越臼臼� 臼臼憶臼         �
*�                                                敖陳陳陳朕        �
*�        [X] ��牆竅 ....:....!....:.     �ィ�キ� �....:...�        �
*�                                                青陳陳陳潰        �
*楽陳 �祗�罔����讚�� ��Л�腑�┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!....崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�      敖 PDF ���� 陳陳陳陳陳陳陳陳陳陳朕 敖���� � ├�÷.陳�       �
*�      �....:....!....:....!....:....!..� �....:....!....:.�       �
*�      青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 青陳陳陳陳陳陳陳潰       �
*�                 < OK Ctrl-W >< �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*�                             < OK >                               �

*------------------------------------------------------------------------

PUSH MENU _MSYSMENU
SET SYSMENU TO

SET SYSMENU AUTOMATIC

DEFINE PAD _Edit OF _MSYSMENU PROMPT "�����皋牀��碎" COLOR SCHEME 4
ON PAD _Edit OF _MSYSMENU ACTIVATE POPUP _Edit
DEFINE POPUP _Edit MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR _MED_UNDO  OF _Edit PROMPT "�皙キ�"     KEY Ctrl+Z, "Ctrl-Z"
DEFINE BAR _MED_CUT   OF _Edit PROMPT "�諤ェ�碎"   KEY Ctrl+X, "Ctrl-X"
DEFINE BAR _MED_COPY  OF _Edit PROMPT "����牀��碎" KEY Ctrl+C, "Ctrl-C"
DEFINE BAR _MED_PASTE OF _Edit PROMPT "���※碎"    KEY Ctrl+V, "Ctrl-V"

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!

DO Prp_Nav_2
DO D_Win_N WITH 23, 70, "�������皀�讚�� ┃筮爼�罔� �� ��Ж罔�"
DO Sun_Bord WITH  6,  2, 17, 67, " �祗�罔����讚�� ��Л�腑�┘ "
DO Sun_Bord WITH 18,  8, 20, 41, " PDF ���� "
DO Sun_Bord WITH 18, 43, 20, 60, "���� � ├�÷."

*      �〓� ���ォ ゛����
*
m.tmpName = ALLTRIM(ST_4404.PREFIX)+ALLTRIM(ST_4404.NAME)+" "+ALLTRIM(ST_4404.PRODUCER)
m.tmpName = PADR(m.tmpName, WCOLS()-12)
@  2, 2 SAY "��Ж罔�"
@  2, COL()+1 GET m.tmpName
*@  2, COL()+1 GET ST_4404.PREFIX
*@  2, COL()+1 GET ST_4404.NAME
*@  2, COL()+1 GET ST_4404.PRODUCER
CLEAR GETS
@  4, 10 GET swCorp PICTURE "@*C ��牆竅" VALID G_Corp()
@  4, 21 GET tmpCorpN WHEN .F.
@  4, 42 SAY "�ィ�キ�"
@  3, 50 GET tmpSegment FROM segList PICTURE "@^" SIZE 1, 10
IF stop_corr
  CLEAR GETS
  @  7,  3 EDIT m.tmpFunc SIZE 10, 63 NOMODIFY
  @ 19,  9 SAY  m.tmpPdf COLOR SCHEME 1
  @ 19, 44 SAY  m.tmpImage COLOR SCHEME 1
  @ WROWS()-2, 31 GET ex PICTURE "@*HT \! OK "
ELSE
  @  7,  3 EDIT tmpFunc SIZE 10, 63
  @ 19,  9 GET  m.tmpPdf
  @ 19, 44 GET  m.tmpImage
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
ENDIF
READ CYCLE

IF ex = 1
  REPLACE COR_CODE  WITH tmpCorpC, ;
          CORPUS    WITH tmpCorpN, ;
          FUNCTION  WITH tmpFunc,  ;
          DATASHEET WITH tmpPdf,   ;
          IMAGE     WITH tmpImage, ;
          SEGMENT   WITH segList(tmpSegment)
ENDIF

*--------------------------------------------------------------------------

SET SYSMENU OFF
POP MENU _MSYSMENU

POP KEY
RELEASE WINDOW (win_name)

USE IN ST_4404
USE IN SUB_4404
SELECT (sel_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Corp       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� ��牆竅�.                                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.04.2004 陳�
PROCEDURE G_Corp

DO Get_Corp WITH tmpCorpC, tmpCorpN
swCorp = .F.
SHOW GET tmpCorpN
SHOW GET swCorp

RETURN .T.
