*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ 竅����→� ������.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Set_Bnd

*        �痰���→� ����� か� �爬�. �爐ム��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                    �
*�                    [ ] ｡���                       �
*�                                                    �
*� �ア�� ������� ($) 99999   ������ ������� ($) 99999 �
*�                                                    �
*�             ( ) ��Г�碎 ( ) ���艪�Θ碎             �
*�                                                    �
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >          �
*�                                                    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE exi, is_new, is_com, bound_l, bound_r, ex, crea, w
crea    = 1
bound_l = 0
bound_r = 0
ex      = 1
is_com  = .F.
w       = .F.
IF File_O(prmo_path+"BND.MEM")
  RESTORE FROM (prmo_path+"BND.MEM") ADDITIVE
ELSE
  bound_l = 0
  bound_r = 0
ENDIF

DO D_W WITH "Set_Bnd", 12, 56, "�痰���→� ����� か� �爬�. �爐ム��", 1

@ 3, 22 GET is_com PICTURE "@*C ｡���" WHEN Sav_W() VALID Do_Bnd()
@ 5,  3 SAY "�ア�� ������� ($)"  GET bound_l PICTURE "@Z 99999"
@ 5, 29 SAY "������ ������� ($)" GET bound_r PICTURE "@Z 99999"
@ 7, 15 GET crea PICTURE "@*RH ��Г�碎 ;���艪�Θ碎"
@ 9, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Sav_Val()

RELEASE WINDOW Set_Bnd
RETURN

**************************************************************************
*
*   ��縲��錺� √�き�� Л�腑�┘
*
PROCEDURE Sav_W

w = is_com
RETURN .T.

**************************************************************************
*
*   ��瘁����←│�ガ �����肓
*
PROCEDURE Do_Bnd

IF w = is_com
  RETURN .T.
ENDIF

IF is_com
  IF File_O(base_path+"BND.MEM")
    RESTORE FROM (base_path+"BND.MEM") ADDITIVE
  ELSE
    bound_l = 0
    bound_r = 0
  ENDIF
ELSE
  IF File_O(prmo_path+"BND.MEM")
    RESTORE FROM (prmo_path+"BND.MEM") ADDITIVE
  ELSE
    bound_l = 0
    bound_r = 0
  ENDIF
ENDIF
SHOW GET bound_l
SHOW GET bound_r
RETURN .T.

**************************************************************************
*
*   ��縲��錺� 皰, 艪� ����□�え��
*
PROCEDURE Sav_Val

IF ex = 2
  RETURN .T.
ENDIF

IF crea = 2
  IF is_com
    IF File_O(base_path+"BND.MEM")
      DELETE FILE (base_path+"BND.MEM")
    ENDIF
  ELSE
    IF File_O(prmo_path+"BND.MEM")
      DELETE FILE (prmo_path+"BND.MEM")
    ENDIF
  ENDIF
ELSE
  IF is_com
    SAVE TO (base_path+"BND.MEM") ALL LIKE bound_*
  ELSE
    SAVE TO (prmo_path+"BND.MEM") ALL LIKE bound_*
  ENDIF
ENDIF
RETURN .T.
