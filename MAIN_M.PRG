*����������������������������������������������������������������������ͻ
*�                                                                      �
*�           �ணࠬ�� ᮧ����� �᭮����� ���� �ਫ������.              �
*�                                                                      �
*����������������������������������������������������������������������ͼ
PROCEDURE Main_M
PARAMETERS m_file, a_level, sch_num
*
*   m_file  - ��� ( �, ��������, ���� ) 䠩�� ���ᠭ�� ����;
*   a_level - �஢��� ����㯠;
*   sch_num - 梥⮢�� �奬�.
*
PRIVATE sch_w, s0, sw, lv1, lv2

*
*   ��⠭�������� 梥⮢�� �奬�
*
sch_w = IIF(TYPE("sch_num") = "N", sch_num, 1)

*
*   ���뢠�� 䠩� ���ᠭ�� ���� � ���祬 �㭪�� ᮣ��᭮ ᠭ���
*
USE (m_file) ORDER TAG LEVELS
SET FILTER TO IIF(a_level=0, .T., EMPTY(SUBSTR(ACC_LABELS,a_level,1))   ;
              .AND. .NOT. H_FOR_ALL )
DELETE FILE (tmpo_path+"MENU_LST.DBF")
COPY TO (tmpo_path+"MENU_LST.DBF")
USE (tmpo_path+"MENU_LST.DBF")
*
*  ����塞 �㭪�� ��ண� �஢�� ᮮ⢥�����騥 ����� �������.
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
*  ��ନ�㥬 ����...
*
DEACTIVATE MENU Main_M
RELEASE MENU Main_M EXTENDED
DEFINE MENU Main_M BAR COLOR SCHEME (sch_w)
PRIVATE pl1, bl2, bl3, mn2, mn3
SCAN
  DO CASE
  CASE EMPTY(LEVEL2)                 && �㭪� ��ࢮ�� �஢��
    pl1 = "PAD1_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Main_M PROMPT ALLTRIM(P_PROMPT)+" "
    IF EMPTY(COMM_STR)                   && ����� ���� �������
      mn2 = "POP"+ALLTRIM(STR(LEVEL1))
      ON PAD (pl1) OF Main_M ACTIVATE POPUP (mn2)
      DEFINE POPUP (mn2) MARGIN RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl2 = 1
    ELSE                                && ����� �ணࠬ��
      s0 = "DO Start_P WITH '"+ALLTRIM(COMM_STR)+"'"
      ON SELECTION PAD (pl1) OF Main_M &s0
    ENDIF
  CASE EMPTY(LEVEL3)                 && �㭪� ��ண� �஢��
    DEFINE BAR bl2 OF (mn2) PROMPT ALLTRIM(P_PROMPT)+" "
    IF EMPTY(COMM_STR)                   && ����� ���� �������
      mn3 = "POP"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      ON BAR bl2 OF (mn2) ACTIVATE POPUP (mn3)
      DEFINE POPUP (mn3) MARGIN RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl3 = 1
    ELSE                                && ����� �ணࠬ��
      s0 = "DO Start_P WITH '"+ALLTRIM(COMM_STR)+"'"
      ON SELECTION BAR bl2 OF (mn2) &s0
    ENDIF
    bl2 = bl2+1
  OTHERWISE    && �㭪� ���쥣� �஢�� � ������� �������!
    DEFINE BAR bl3 OF (mn3) PROMPT ALLTRIM(P_PROMPT)+" "
    s0 = "DO Start_P WITH '"+ALLTRIM(COMM_STR)+"'"
    ON SELECTION BAR bl3 OF (mn3) &s0
    bl3 = bl3+1
  ENDCASE
ENDSCAN

USE
RETURN  