*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� For_OG       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          蹍��Θ �� ��┘����.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.11.2001 様�
PROCEDURE For_OG

PRIVATE m1, y1, m2, y2, c_code, c_name, tmpName, dep_c, yTmp, mTmp
m1 = MONTH(GOMONTH(DATE(),-3))
y1 = YEAR(GOMONTH(DATE(),-3))
m2 = MONTH(DATE())
y2 = YEAR(DATE())
dep_c = 2
c_code = 0
c_name = SPACE(50)
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
   (  CUS_CODE  N( 8),   ;
      CUS_NAME  C(50),   ;
      MONTH     N( 2),   ;
      YEAR      N( 4),   ;
      SUM_OWN   N(13,4), ;
      SUM_OTHER N(13,4)  )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON STR(CUS_CODE)+STR(YEAR)+STR(MONTH) TAG TMP
SET ORDER TO

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_LIST
SET ORDER TO

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Calc OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Calc OF (menu_name) DO Sw_Mode WITH "Calc"

DEFINE PAD Save OF (menu_name) PROMPT "��縲���碎" KEY Ctrl-A, "F2"
ON SELECTION PAD Save OF (menu_name) DO Sw_Mode WITH "Save"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 71
*敖��┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳���つ�翊陳陳陳��楉陳陳��痰. �發カ訖
*蟹���������������������������������� ���� �� 99999999.9999 99999999.9999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
DO D_Wins WITH ln, wd, "蹍��Θ �� ��┘����", 0, 1
@ 1, 2 SAY "���┘�眥陳陳陳陳陳陳陳陳陳陳陳陳陳陳���つ�翊陳陳陳��楉陳陳��痰. �發カ�"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2    DO Sw_Mode WITH "Save"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CUS_NAME:35:H="",    ;
                  MONTH:H="",          ;
                  YEAR:H="",           ;
                  SUM_OWN:H="",        ;
                  SUM_OTHER,           ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Calc")
    ENDIF

  CASE what_do = "Calc"    && ��瘁腑�

    DO C_Form
    what_do = "List"

  CASE what_do = "Save"    && ��縲��キ┘

    DO Save_DBF
    what_do = "List"

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
SELECT TMP_LIST
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3), "cdx")
SELECT (s_sav)

RETURN
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-2, 5 SAY CUS_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� C_Form       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��爼� か� ��瘍モ�                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.11.2001 陳�
PROCEDURE C_Form

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 �ム┏� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �     � 99 �メ閹 9999 ��� �� 99 �メ閹 9999 ���     � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 [ ] ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, sw_cus
ex = 1
sw_cus = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 58, "��瘍モ �牀���"
DO Sun_Bord WITH  2,  3,  4, 54, " �ム┏� "
DO Sun_Bord WITH  5,  3,  7, 54, " [ ] ��┘�� "
@ 6, 4 SAY c_name

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  9 SAY "� 99 �メ閹 9999 ��� �� 99 �メ閹 9999 ���"
@ 3, 11 GET m1 RANGE 1, 12 PICTURE "99"
@ 3, 20 GET y1 RANGE YEAR(DATE())-10 PICTURE "9999"
@ 3, 32 GET m2 RANGE 1, 12 PICTURE "99"
@ 3, 41 GET y2 RANGE YEAR(DATE()) PICTURE "9999"
@ 5,  6 GET sw_cus PICTURE "@*C ��┘��" VALID Get_C()

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  yTmp = y1
  mTmp = m1
  SELECT TMP_LIST
  DO WHILE yTmp*12+mTmp <= y2*12+m2
    APPEND BLANK
    REPLACE CUS_CODE WITH c_code, ;
            CUS_NAME WITH c_name, ;
            YEAR     WITH yTmp,   ;
            MONTH    WITH mTmp
    mTmp = mTmp+1
    IF mTmp > 12
      yTmp = yTmp+1
      mTmp = 1
    ENDIF
  ENDDO
  SET ORDER TO TAG TMP
  SELECT 0
  USE (base_path+"PERSONS") ORDER TAG CODE
  SELECT 0
  USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE
  SET RELATION TO WHO INTO PERSONS
  mTmp = y1*12+m1
  yTmp = y2*12+m2
  SEEK c_code
  SCAN REST WHILE CUS_CODE = c_code
    IF SEEK(STR(c_code)+STR(YEAR(DOC_DATE))+STR(MONTH(DOC_DATE)), "TMP_LIST")
      IF PERSONS.DEP_NO = dep_c
        REPLACE TMP_LIST.SUM_OWN WITH TMP_LIST.SUM_OWN+TOTAL_D
      ELSE
        REPLACE TMP_LIST.SUM_OTHER WITH TMP_LIST.SUM_OTHER+TOTAL_D
      ENDIF
    ENDIF
  ENDSCAN
  USE
  SELECT PERSONS
  USE
  SELECT TMP_LIST
  SET ORDER TO
  GO BOTTOM
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_C        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �襦�� ��┘���                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.11.2001 陳�
PROCEDURE Get_C

IF .NOT. sw_cus
  RETURN .T.
ENDIF
sw_cus = .F.
SHOW GET sw_cus
DO Customer WITH c_code, c_name, "i"
@ 6, 4 SAY c_name

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ���※�讚�痰� ������キ�� ゛����.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.11.2001 陳�
PROCEDURE Tst_Blank

PRIVATE r_sav, mss

IF ex # 1
  RETURN .T.
ENDIF

IF y2*12+m2 < y1*12+m1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �オ�玻オ皚� 礫����� �ム┏�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� ��┘���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

SELECT TMP_LIST
r_sav = RECNO()
SET ORDER TO TAG TMP
IF SEEK(STR(c_code))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰� ��┘�� 礒� メ碎 � 甎瓱��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SET ORDER TO
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
  RETURN .F.
ENDIF

SET ORDER TO
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Save_DBF     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��縲��キ┘ �����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.11.2001 陳�
PROCEDURE Save_DBF

PRIVATE tmp_dir, mss, tmpName, r_sav

tmp_dir = IIF(Is_Dir("D:"), "D:\", "C:\")

DO WHILE .T.
  tmpName = New_File(tmp_dir, "dbf", "���Θ皀 ����")
  IF EMPTY(tmpName)
    RETURN
  ENDIF
  IF File_O(tmpName)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ����� ���� 礒� 痺薀痰≪モ. ����━? [ �� ] [ �モ ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      DELETE FILE (tmpName)
    ELSE
      LOOP
    ENDIF
  ENDIF
  EXIT
ENDDO

r_sav = RECNO()
COPY TO (tmpName)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN
