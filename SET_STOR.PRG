*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� ��⠭���� ���. ������.                      �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Set_Stor

PRIVATE typ, ex, acc_c

*     ��⠭����� ���. ����稥 ࠢ��:
*���������������������������������������Ŀ
*�   ( ) �㬬�୮�� ������ � 䨫�����   �
*�   ( ) �㬬�୮� �஭�                 �
*�   ( ) ������ � ����� �� 䨫�����     �
*�   < OK Ctrl-W > < �⪠������ Esc >    �
*�����������������������������������������

HIDE MENU Main_M
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
typ = 1
ex  = 1

DO D_W WITH "Set_Stor", 7, 43, "��⠭����� ���. ����稥 ࠢ��:", 1

@ 2, 5 GET typ PICTURE   ;
"@*R �㬬�୮�� ������ � 䨫�����; �㬬�୮� �஭�; ������ � ����� �� 䨫�����"
@ 5, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
READ CYCLE

RELEASE WINDOW Set_Stor
IF ex = 2
  RETURN
ENDIF

USE (base_path+"STOCK")
DO CASE
CASE typ = 1  && ��� �������
  DO Wt_Mess WITH "��⠭���� ���. ������..."
  SCAN
    REPLACE QNT_STORE WITH QNT_REAL
  ENDSCAN
  DO Wt_Mess
CASE typ = 2  && ��� �����
  DO Wt_Mess WITH "��⠭���� ���. ������..."
  SCAN
    REPLACE QNT_STORE WITH QNT_SAVE+QNT_REZ
  ENDSCAN
  DO Wt_Mess
CASE typ = 3  && ������� 䨫���
  DO Use_Dummy
  DO Use_Link
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
  IF Custs("", .F., .F.)
    acc_c = ACCOUNT.CUS_CODE
    DO Wt_Mess WITH "��⠭���� ���. ������..."
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