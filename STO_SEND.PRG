*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Sto_Send     ��о�｀砒┴ ���┃� ���矗┘���                 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      �ムイ��� ����� � DBF-筮爼�皀.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.11.2001 様�
PROCEDURE Sto_Send
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


s_sav = SELECT()

PRIVATE s_n, s_name, s_list, s_fl

SELECT 0
USE (base_path+"TYP_SEND")
n_s = 0
SCAN
  n_s = n_s + 1
  DIMENSION s_name(n_s), s_fl(n_s)
  s_name(n_s) = NOTE
  s_fl(n_s)   = ALLTRIM(FILE_STO)
ENDSCAN

IF n_s = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� 皋�� ��瘠���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

s_list = 1
*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�               敖陳陳陳陳陳陳陳陳陳陳陳陳陳�    �
*�  �┓� ��瘠��� � ....:....:....:....:....: �    �
*�               青陳陳陳陳陳陳陳陳陳陳陳陳陳�    �
*�           敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� [ ] ����  � ....:....:....:....:....:....:.. � �
*�           青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�          < OK Ctrl-W >< �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE is_file, ex, sav_file
is_file = .F.
ex = 1
sav_file = ALLTRIM(s_fl(s_list))
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 52, "���キ���矣�� � DBF-筮爼�皀"
@ 3, 4 SAY "�┓ ��瘠���"

*      �〓� ���ォ ゛����

@ 2,17 GET s_list   PICTURE "@^" FROM s_name SIZE 1, 25 VALID Set_File()
@ 6, 3 GET is_file  PICTURE "@*C ����" VALID Ch_File()
@ 6,12 EDIT sav_file SIZE 1, 25 WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Send()

IF ex = 1
  DO Send_Ch WITH s_list
ENDIF

USE
SELECT (s_sav)
POP KEY
RELEASE WINDOW(win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      鉘� か� �襦������� 皋�� ��瘠���                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 21.11.2001 夕
PROCEDURE Set_File
sav_file = ALLTRIM(s_fl(s_list))
SHOW GET sav_file
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      鉘� か� �襦������� 皋�� ��瘠���                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 21.11.2001 夕
PROCEDURE Ch_File
PRIVATE tmpPth, tmpName, i, tmp
IF .NOT. is_file
  RETURN .T.
ENDIF  
i = RAT("\",sav_file)
tmpPth = "C:\"
tmpName = ""
IF i > 0 
  tmpPth = ALLTRIM(LEFT(sav_file,i-1))
  tmpName = ALLTRIM(SUBSTR(sav_file,i+1))
ENDIF
tmp = New_File(tmpPth, "DBF", "�ぅ 甌縲���碎 爛с�赳��?", 0, tmpName)   
IF .NOT. EMPTY(tmp)
  sav_file = ALLTRIM(tmp)
  SHOW GET sav_file
ENDIF
is_file = .F.  
SHOW GET is_file
RETURN .T.
  
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             ��瘠��� !!!                                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 20.11.2001 夕
PROCEDURE Send_Ch
PARAMETERS s_fld

PRIVATE n_fld, sav_pth
n_fld = ""
SELECT TYP_SEND
GO s_fld
n_fld = ALLTRIM(FIELD_SEND)

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME
COPY FIELDS &n_fld TO (sav_file)
USE
SELECT TYP_SEND
REPLACE FILE_STO WITH sav_file
USE
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             蹍▲爲�.                                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 20.11.2001 夕
PROCEDURE Tst_Send
IF ex = 1
  IF File_O(sav_file)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ����� ���� 礒� メ碎, 磴��━? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
  DELETE FILE (sav_file)
  ENDIF
ENDIF  

SELECT TYP_SEND
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� 皋�� ��瘠���..."
  mss(3) = ""
  IF .NOT. mute
    DO Out_Mess WITH 7, "mss"
  ENDIF
  SELECT (s_sav)
  RETURN
ENDIF

RETURN