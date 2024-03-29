*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                      �
*�           蹍������ 甌Г���� �甅�↓��� �キ� �爬��Ε���.              �
*�                                                                      �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Modi_M_M
PARAMETERS m_file, a_level, sch_num
*
*   m_file  - ━� ( �, 〓К�Ν�, �竄� ) ����� �������� �キ�;
*   a_level - 窶�▲�� ぎ痰祚�;
*   sch_num - 罌モ���� 痳ガ�.
*
PRIVATE sch_w
PRIVATE pad_sav, bar_, popup_n
pad_sav  = ""
bar_     = 0
popup_n  = ""

*
*   �痰���←│�ガ 罌モ�≪� 痳ガ�
*
sch_w = IIF(TYPE("sch_num") = "N", sch_num, 1)

*
*   �皖琺��ガ ���� �������� �キ� � �瑙腑� �祗�硅 甌���甅� ����罔鍖
*
USE (m_file) ORDER TAG LEVELS
SET FILTER TO .NOT. H_FOR_ALL

*
*  ��爼�珮ガ �キ�...
*
DEFINE MENU Menu_T BAR MARK "�" COLOR SCHEME (sch_w)
PRIVATE pl1, bl2, bl3, mn2, mn3
SCAN
  DO CASE

  CASE EMPTY(LEVEL2)                 && 釪�� �ム〓�� 窶�↓�
    pl1 = "PAD1_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      mn2 = "POP"+ALLTRIM(STR(LEVEL1))
      ON PAD (pl1) OF Menu_T ACTIVATE POPUP (mn2)
      DEFINE POPUP (mn2) MARGIN MARK "�" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl2 = 1
    ELSE                                && �痰���※碎/甅閧� ���モ��
      SET MARK OF PAD (pl1) OF Menu_T TO SUBSTR(ACC_LABELS,a_level,1) # " " 
      ON SELECTION PAD (pl1) OF Menu_T DO Mark_P
    ENDIF

  CASE EMPTY(LEVEL3)                 && 釪�� ≒�牀�� 窶�↓�
    DEFINE BAR bl2 OF (mn2) PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      mn3 = "POP"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      ON BAR bl2 OF (mn2) ACTIVATE POPUP (mn3)
      DEFINE POPUP (mn3) MARGIN MARK "�" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl3 = 1
    ELSE                                && �痰���※碎/甅閧� ���モ��
      SET MARK OF BAR bl2 OF (mn2) TO SUBSTR(ACC_LABELS,a_level,1) # " "
      ON SELECTION BAR bl2 OF (mn2) DO Mark_B
    ENDIF
    bl2 = bl2+1

  OTHERWISE    && 釪�� 矗モ譽�� 窶�↓� � �┴���� ��がキ�!
    DEFINE BAR bl3 OF (mn3) PROMPT ALLTRIM(P_PROMPT)+" "
    SET MARK OF BAR bl3 OF (mn3) TO SUBSTR(ACC_LABELS,a_level,1) # " "
    ON SELECTION BAR bl3 OF (mn3) DO Mark_B
    bl3 = bl3+1
  ENDCASE

ENDSCAN

*
*   ��皀���皋↓�� 竅����→� ���モ�� � �キ�
*
SET ESCAPE OFF
DO WHILE .T.
  IF EMPTY(bar_) .AND. EMPTY(popup_n) .AND. EMPTY(pad_sav)
    ACTIVATE MENU Menu_T
  ELSE
    IF EMPTY(bar_)
      ACTIVATE MENU Menu_T PAD (pad_sav)
    ELSE
      ACTIVATE POPUP (popup_n) BAR bar_
    ENDIF
  ENDIF
  IF MOD(READKEY(),256) = 12
    EXIT
  ENDIF
  pad_sav = PAD()
ENDDO

*
*  �痰���→� ���モ�� � ��Д ����諷 �� ���モ��� �キ�: �ム�覃 轤��
*                ���モ�� か� 皀爼��讚諷 �祗�皰�
*
SCAN
  DO CASE

  CASE EMPTY(LEVEL2)                 && 釪�� �ム〓�� 窶�↓�
    pl1 = "PAD1_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
      mn2 = "POP"+ALLTRIM(STR(LEVEL1))
      bl2 = 1
    ELSE                                && �痰���※碎/甅閧� ���モ��
      bar_ = IIF(MRKPAD("Menu_T",pl1), "*", " ")
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
    ENDIF

  CASE EMPTY(LEVEL3)                 && 釪�� ≒�牀�� 窶�↓�
    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
      mn3 = "POP"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      bl3 = 1
    ELSE                                && �痰���※碎/甅閧� ���モ��
      bar_ = IIF(MRKBAR(mn2,bl2), "*", " ")
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
    ENDIF
    bl2 = bl2+1

  OTHERWISE    && 釪�� 矗モ譽�� 窶�↓� � �┴���� ��がキ�!
    bar_ = IIF(MRKBAR(mn3,bl3), "*", " ")
    REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
    bl3 = bl3+1
  ENDCASE

ENDSCAN

*
*  �痰���→� ���モ�� � ��Д ����諷 �� ���モ��� �キ�: ≒�牀� 轤��
*                ���モ�� か� �モム���讚諷 �祗�皰�
*

SET FILTER TO IIF(a_level=0, .T., EMPTY(SUBSTR(ACC_LABELS,a_level,1)))  ;
              .AND. .NOT. H_FOR_ALL

SET ORDER TO TAG LEVELS DESCENDING
PRIVATE lv1, lv2, lv3, ok
STORE 0 TO lv1, lv2, lv3

SCAN

  ok = .F.
  IF EMPTY(COMM_STR)        && �� 皀爼, ��ぎ ��М÷�碎瘴
    IF EMPTY(LEVEL2)       && �ム�覃 窶�▲��
      IF .NOT. EMPTY(lv2) .AND. EMPTY(lv3)
        IF LEVEL1 = lv1
          ok = .T.
        ENDIF
      ENDIF
    ELSE                   && ≒�牀� 窶�▲��
      IF .NOT. EMPTY(lv3)
        IF LEVEL1 = lv1 .AND. LEVEL2 = lv2
          ok = .T.
        ENDIF
      ENDIF
    ENDIF
    IF .NOT. ok
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,"*")
      LOOP
    ENDIF
  ENDIF
  lv1 = LEVEL1
  lv2 = LEVEL2
  lv3 = LEVEL3
ENDSCAN

USE
RETURN  

PROCEDURE Mark_P

SET MARK OF PAD (PAD()) OF Menu_T TO .NOT. MRKPAD("Menu_T",PAD())
*popup_n = ""
popup_n = POPUP()
pad_sav = ALLTRIM(PROMPT())
bar_ = BAR()

RETURN

PROCEDURE Mark_B

SET MARK OF BAR BAR() OF (POPUP()) TO .NOT.MRKBAR(POPUP(),BAR())
popup_n = POPUP()
*pad_sav = ""
pad_sav = ALLTRIM(PROMPT())
bar_ = BAR()

RETURN
