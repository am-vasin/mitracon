*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ ������キ�� �矗─竄�� ��┘���.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Wk_Fill
PARAMETERS ext_menu
PRIVATE i, s, lst, crsr

IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    DEACTIVATE MENU (ext_menu)
  ENDIF
ENDIF

crsr = SET("CURSOR") = "OFF"
SET CURSOR ON

DEFINE WINDOW H_Wind FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Wind

DIMENSION lst(4)

lst(1) = "��珥�腦� か� ��爲モ┃��    "
lst(2) = "�矗─竄� か� 瘍モ�-���矣琺 "
lst(3) = "��瘡�                      "
lst(4) = "�諷�� ( Esc )              "

DO D_W WITH "Wk_Fill", 7, LEN(lst(1))+4, "�皰 ＜ぅ� ������閧�?", 0

i = 1
DO WHILE .T.
  ACTIVATE WINDOW Wk_Fill SAME
  i = MAX(i, 1)
  i = MIN(i, 4)
  @ 1,1 MENU lst, 4
  READ MENU TO i
  s = READKEY() % 256
  IF s = 12
    EXIT
  ENDIF
  IF s # 15
    LOOP
  ENDIF
  IF i = 1
    DO Cli_Info WITH "", .T.
    LOOP
  ENDIF
  IF i = 2
    DO Cus_Bil WITH ACCOUNT.CUS_CODE, ACCOUNT.CUS_NAME
    LOOP
  ENDIF
  IF i = 3
    DO Dossier WITH ACCOUNT.CUS_CODE, "C"
    LOOP
  ENDIF
  EXIT
ENDDO

RELEASE WINDOW Wk_Fill
RELEASE WINDOW H_Wind
IF crsr
  SET CURSOR OFF
ENDIF

RETURN
