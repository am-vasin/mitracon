*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� St_Send1     ��о�｀砒┴ ��むォ ��瓱�           27.06.97 10:02:27 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         �ムメ覊�� 痰����珥���� 甎�瓷� ������ � �甅. �����お�.          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE St_Send1
PRIVATE mss, wk_char, pthw

IF (sys_char # "A" .AND. sale_mode # "Mitin")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 �� �甅�↓�� �����お�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

wk_char = ""
DO Ch_Place WITH wk_char

DO Prep_TD    && ��Г�ガ ┼� �腮��ガ 矗��Ж皚竡 え爛�皰爬�
pthw = tmpo_path+"transmit\"

DO Prep_St1 WITH wk_char, pthw

DO Arch WITH pthw, "ST_SEND1"

PRIVATE out_path
n = ADIR(arr_w,pthw+"*.*")
FOR i = 1 TO n
  IF LEFT(arr_w(i,1),10) # "ST_SEND1.A")
    DELETE FILE (pthw+arr_w(i,1))
  ENDIF
ENDFOR

out_path = "\"
out_path = Int_Dir(out_path, "�磴� ��茱� ����襯?", 0)

IF .NOT. EMPTY(out_path)
  DO Copy_A WITH pthw+"ST_SEND1", out_path+"ST_SEND1"  && ��縲��キ┘ �璢│�
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            �襦�� �����お�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 27.06.97 10:28:47 夕
PROCEDURE Ch_Place
PARAMETER w_code
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy
SELECT 0
USE (base_path+"PLACES")
SET FILTER TO P_CHAR # "A"
GO TOP
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ��� �モ �� �き�� ぎ�����皀�讚�� �����お�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("P_NAME")

DO D_Wins WITH ln, wd, "ç��お�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS P_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    w_code = P_CHAR
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�  ��ぃ�皰→� 痰����珥���� 甎�瓷� ������ か� �ムイ�腮 �� ぎ�. �����お�.  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 27.06.97 10:52:22 夕
PROCEDURE Prep_St1
PARAMETERS w_ch, pth
PRIVATE arr_w, p, i

*
*     ��┘�硅
*
DO Wt_Mess WITH "��┘�硅"
USE (base_path+"ACCOUNT")
COPY STRUCTURE TO (pth+"ACCOUNT")
SELECT 0
USE (pth+"ACCOUNT") ALIAS DEST
SELECT ACCOUNT
SET ORDER TO TAG PL_NAME
SEEK w_ch
SCAN REST WHILE SUBSYS = w_ch
  SCATTER TO arr_w
  SELECT DEST
  APPEND FROM ARRAY arr_w
  SELECT ACCOUNT
ENDSCAN

SET ORDER TO TAG EMP_NAME
SEEK w_ch
SCAN REST WHILE SUBSYS = w_ch
  SCATTER TO arr_w
  SELECT DEST
  APPEND FROM ARRAY arr_w
  SELECT ACCOUNT
ENDSCAN
DO Wt_Mess
DO Wt_Mess WITH "���甌� ��｀腮� �メ�"

*
*     ���甌� ��｀腮� �メ�
*
USE 
SELECT DEST
USE (base_path+"ARM_LST")
COPY TO (pth+"ARM_LST")

*
*     ���甌� �發カ�� �爛く爬閧��
*
DO Wt_Mess
DO Wt_Mess WITH "�發カ� �爛く爬閧��"
USE (base_path+"DEPS")
COPY TO (pth+"DEPS")

*
*     ���甌� 皋��� 皰����
*
DO Wt_Mess
DO Wt_Mess WITH "�發カ� �爛く爬閧��"
USE (base_path+"GOODS")
COPY TO (pth+"GOODS")

*
*     ���甌� 甌矗磴�┴�� �爛く爬閧��
*
DO Wt_Mess
DO Wt_Mess WITH "���甌� 甌矗磴�┴�� �爛く爬閧��"
USE (base_path+"PERSONS")
COPY STRUCTURE TO (pth+"PERSONS")
SELECT 0
USE (pth+"PERSONS") ALIAS DEST
SELECT PERSONS
i = 1
p = 0
DO WHILE .NOT. EMPTY(FIELD(i))
  IF FIELD(i) == "SALARY"
    p = i
    EXIT
  ENDIF
  i = i+1
ENDDO
SCAN
  SCATTER TO arr_w
  IF p # 0
    arr_w(p) = ""
  ENDIF
  SELECT DEST
  APPEND FROM ARRAY arr_w
  SELECT PERSONS
ENDSCAN
DO Wt_Mess

CLOSE DATABASES

RETURN
