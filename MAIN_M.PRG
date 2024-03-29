*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                      �
*�           蹍������ 甌Г���� �甅�↓��� �キ� �爬��Ε���.              �
*�                                                                      �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Main_M
PARAMETERS m_file, a_level, sch_num
*
*   m_file  - ━� ( �, 〓К�Ν�, �竄� ) ����� �������� �キ�;
*   a_level - 窶�▲�� ぎ痰祚�;
*   sch_num - 罌モ���� 痳ガ�.
*
PRIVATE sch_w, s0, sw, lv1, lv2

*
*   �痰���←│�ガ 罌モ�≪� 痳ガ�
*
sch_w = IIF(TYPE("sch_num") = "N", sch_num, 1)

*
*   �皖琺��ガ ���� �������� �キ� � �瑙腑� �祗�硅 甌���甅� ����罔鍖
*
USE (m_file) ORDER TAG LEVELS
SET FILTER TO IIF(a_level=0, .T., EMPTY(SUBSTR(ACC_LABELS,a_level,1))   ;
              .AND. .NOT. H_FOR_ALL )
DELETE FILE (tmpo_path+"MENU_LST.DBF")
COPY TO (tmpo_path+"MENU_LST.DBF")
USE (tmpo_path+"MENU_LST.DBF")
*
*  ����錺� �祗�硅 ≒�牀�� 窶�↓� 甌�癶モ痰≪鉗┘ �竅硅� ��がキ�.
*
SCAN FOR .NOT. EMPTY(LEVEL2) .AND. EMPTY(COMM_STR)
  s0 = RECNO()
  lv1 = LEVEL1
  lv2 = LEVEL2
  SKIP
  IF BOF() .OR. EOF()
    sw = .F.
  ELSE
    sw = lv1 = LEVEL1 .AND. lv2 = LEVEL2
  ENDIF
  GO s0
  IF .NOT. sw
    DELETE
  ENDIF
ENDSCAN

SCAN FOR .NOT. EMPTY(LEVEL1) .AND. EMPTY(COMM_STR)
  s0 = RECNO()
  lv1 = LEVEL1
  SKIP
  IF BOF() .OR. EOF()
    sw = .F.
  ELSE
    sw = lv1 = LEVEL1
  ENDIF
  GO s0
  IF .NOT. sw
    DELETE
  ENDIF
ENDSCAN

*
*  ��爼�珮ガ �キ�...
*
DEACTIVATE MENU Main_M
RELEASE MENU Main_M EXTENDED
DEFINE MENU Main_M BAR COLOR SCHEME (sch_w)
PRIVATE pl1, bl2, bl3, mn2, mn3
SCAN
  DO CASE
  CASE EMPTY(LEVEL2)                 && 釪�� �ム〓�� 窶�↓�
    pl1 = "PAD1_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Main_M PROMPT ALLTRIM(P_PROMPT)+" "
    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      mn2 = "POP"+ALLTRIM(STR(LEVEL1))
      ON PAD (pl1) OF Main_M ACTIVATE POPUP (mn2)
      DEFINE POPUP (mn2) MARGIN RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl2 = 1
    ELSE                                && ���竅� �牀������
      s0 = "DO Start_P WITH '"+ALLTRIM(COMM_STR)+"'"
      ON SELECTION PAD (pl1) OF Main_M &s0
    ENDIF
  CASE EMPTY(LEVEL3)                 && 釪�� ≒�牀�� 窶�↓�
    DEFINE BAR bl2 OF (mn2) PROMPT ALLTRIM(P_PROMPT)+" "
    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      mn3 = "POP"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      ON BAR bl2 OF (mn2) ACTIVATE POPUP (mn3)
      DEFINE POPUP (mn3) MARGIN RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl3 = 1
    ELSE                                && ���竅� �牀������
      s0 = "DO Start_P WITH '"+ALLTRIM(COMM_STR)+"'"
      ON SELECTION BAR bl2 OF (mn2) &s0
    ENDIF
    bl2 = bl2+1
  OTHERWISE    && 釪�� 矗モ譽�� 窶�↓� � �┴���� ��がキ�!
    DEFINE BAR bl3 OF (mn3) PROMPT ALLTRIM(P_PROMPT)+" "
    s0 = "DO Start_P WITH '"+ALLTRIM(COMM_STR)+"'"
    ON SELECTION BAR bl3 OF (mn3) &s0
    bl3 = bl3+1
  ENDCASE
ENDSCAN

USE
RETURN  
