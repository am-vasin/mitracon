*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ 竅����→� ぎ�. ����腮�.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Set_Stor

PRIVATE typ, ex, acc_c

*     �痰���※碎 ぎ�. ����腮� ��↓覓:
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�   ( ) �祠��爿��� ����腮� � 筥������   �
*�   ( ) �祠��爿�� ÷���                 �
*�   ( ) ����腮� � �き�� ├ 筥������     �
*�   < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

HIDE MENU Main_M
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
typ = 1
ex  = 1

DO D_W WITH "Set_Stor", 7, 43, "�痰���※碎 ぎ�. ����腮� ��↓覓:", 1

@ 2, 5 GET typ PICTURE   ;
"@*R �祠��爿��� ����腮� � 筥������; �祠��爿�� ÷���; ����腮� � �き�� ├ 筥������"
@ 5, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE

RELEASE WINDOW Set_Stor
IF ex = 2
  RETURN
ENDIF

USE (base_path+"STOCK")
DO CASE
CASE typ = 1  && ��� �������
  DO Wt_Mess WITH "�痰���→� ぎ�. ����腮�..."
  SCAN
    REPLACE QNT_STORE WITH QNT_REAL
  ENDSCAN
  DO Wt_Mess
CASE typ = 2  && ��� �����
  DO Wt_Mess WITH "�痰���→� ぎ�. ����腮�..."
  SCAN
    REPLACE QNT_STORE WITH QNT_SAVE+QNT_REZ
  ENDSCAN
  DO Wt_Mess
CASE typ = 3  && ����爛皚覃 筥����
  DO Use_Dummy
  DO Use_Link
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
  IF Custs("", .F., .F.)
    acc_c = ACCOUNT.CUS_CODE
    DO Wt_Mess WITH "�痰���→� ぎ�. ����腮�..."
    USE (base_path+"RYN_L") ORDER TAG CODE
    SET FILTER TO CUS_CODE = acc_c
    SELECT STOCK
    SCAN
      REPLACE QNT_STORE WITH IIF(SEEK(CODE,"RYN_L"), RYN_L.QNT, 0)
    ENDSCAN
    DO Wt_Mess
  ENDIF
ENDCASE

CLOSE DATABASES

RETURN
